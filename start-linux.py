from multiprocessing import Process, Queue
import os
import glob
import shutil
import subprocess
import argparse

def run_a_exe(instance, folder, queue, output_annual_path, output_climate_path):
    print(f"Instance {instance} running in folder {folder}")
    subprocess.run(["./a.exe"], cwd=folder, check=True)

    print(f"Instance {instance} completed.")
    print("Current working directory:", os.getcwd())
    
    # 両方の出力ファイルのパスを設定
    fn_annual_file = os.path.join(folder, "output_annual.txt")
    fn_climate_file = os.path.join(folder, "output_climate.txt")
    
    print(f"fn_annual_file: {fn_annual_file}")
    print(f"fn_climate_file: {fn_climate_file}")

    # output_annual.txt の処理
    if os.path.exists(fn_annual_file):
        # 出力フォルダの作成
        output_annual_folder = os.path.dirname(output_annual_path)
        if not os.path.exists(output_annual_folder):
            os.makedirs(output_annual_folder)
            
        print(f"Copying output_annual.txt to {output_annual_path}")
        shutil.copy(fn_annual_file, output_annual_folder)
        os.rename(os.path.join(output_annual_folder, "output_annual.txt"), output_annual_path)
        os.remove(fn_annual_file)

    # output_climate.txt の処理
    if os.path.exists(fn_climate_file):
        # 出力フォルダの作成
        output_climate_folder = os.path.dirname(output_climate_path)
        if not os.path.exists(output_climate_folder):
            os.makedirs(output_climate_folder)
            
        print(f"Copying output_climate.txt to {output_climate_path}")
        shutil.copy(fn_climate_file, output_climate_folder)
        os.rename(os.path.join(output_climate_folder, "output_climate.txt"), output_climate_path)
        os.remove(fn_climate_file)

    queue.put(folder)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run multiple instances of a process.")
    parser.add_argument(
        "--num_instances",
        type=int,
        required=True,
        help="Number of instances to create and execute.",
    )
    args = parser.parse_args()

    num_instances = args.num_instances
    processes = []
    available_folders = Queue()

    for i in range(num_instances):
        folder = f"./run{i}"
        if not os.path.exists(folder):
            os.makedirs(folder)
        
        template_folder = "template"
        template_files = glob.glob(os.path.join(template_folder, "*"))
        for template_file in template_files:
            shutil.copy(template_file, folder)

        f90_file = os.path.join(folder, "start_point.f90")
        
        if os.path.exists(f90_file):
            try:
                subprocess.run(["gfortran", "start_point.f90", "-o", "a.exe"],
                               check=True,
                               cwd=folder)
                print(f"Compiled start_point.f90 in {folder}")
            except subprocess.CalledProcessError as e:
                print(f"Error compiling start_point.f90 in {folder}: {e}")
        else:
            print(f"Warning: start_point.f90 not found in {folder}")

        available_folders.put(folder)

    # 出力用の親フォルダをタイプごとに設定
    output_base = "/home/ec2-user/sasa_main_code/output"
    output_annual_base = os.path.join(output_base, "output_annual")
    output_climate_base = os.path.join(output_base, "output_climate")
    
    # 入力データ用の親フォルダ
    parent_folders = ["future_c1_conv", "future_c2_conv", "future_c3_conv", "hist_conv"]
    
    for parent in parent_folders:
        # 入力データのパス
        parent_path = f"./{parent}"

        # 出力先フォルダの作成
        output_annual_parent = os.path.join(output_annual_base, parent)
        output_climate_parent = os.path.join(output_climate_base, parent)
        
        if not os.path.exists(output_annual_parent):
            os.makedirs(output_annual_parent)
        if not os.path.exists(output_climate_parent):
            os.makedirs(output_climate_parent)

        subfolders = glob.glob(os.path.join(parent_path, "*"))
        files_to_process = []

        for subfolder in subfolders:
            csv_files = glob.glob(os.path.join(subfolder, "*.csv"))
            files_to_process.extend(csv_files)

        print("Number of files to process:", len(files_to_process))

        for file in files_to_process:
            folder = available_folders.get()
            copied_file = shutil.copy(file, folder)

            new_file_name = os.path.join(folder, 'job.csv')
            if os.path.exists(new_file_name):
                os.remove(new_file_name)
            os.rename(copied_file, new_file_name)

            # ファイル名から相対パスを取得（最初の"./"を削除）
            rel_path = file[2:]
            
            # 出力ファイルのパスを作成
            file_name = os.path.basename(rel_path)
            rel_dir = os.path.dirname(rel_path)
            
            # 各出力ファイルのパスを設定
            output_annual_path = os.path.join(output_annual_base, rel_dir, file_name.replace(".csv", ".txt"))
            output_climate_path = os.path.join(output_climate_base, rel_dir, file_name.replace(".csv", ".txt"))
            
            print(f"output_annual_path: {output_annual_path}")
            print(f"output_climate_path: {output_climate_path}")

            # すでに両方のファイルが存在する場合は処理をスキップ
            if os.path.exists(output_annual_path) and os.path.exists(output_climate_path):
                available_folders.put(folder)
                continue

            # 出力ディレクトリの確認
            annual_output_folder = os.path.dirname(output_annual_path)
            climate_output_folder = os.path.dirname(output_climate_path)
            
            if not os.path.exists(annual_output_folder):
                os.makedirs(annual_output_folder)
            if not os.path.exists(climate_output_folder):
                os.makedirs(climate_output_folder)

            instance = len(processes)
            p = Process(target=run_a_exe, args=(instance, folder, available_folders, output_annual_path, output_climate_path))
            processes.append(p)
            p.start()

        for p in processes:
            p.join()

        processes.clear()

        print(f"All instances for {parent} have completed.")
