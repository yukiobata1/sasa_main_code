from multiprocessing import Process, Queue
import os
import glob
import shutil
import subprocess
import argparse

def run_a_exe(instance, folder, queue, output_file_path):
    print(f"Instance {instance} running in folder {folder}")
    subprocess.run(["./a.exe"], cwd=folder, check=True)

    print(f"Instance {instance} completed.")
    print("Current working directory:", os.getcwd())
    fn_biomass_file = os.path.join(folder, "output_annual.txt")
    print("fn_biomass_file:", fn_biomass_file)

    # The folder path for the final output file
    output_folder = os.path.dirname(output_file_path)

    if os.path.exists(fn_biomass_file):
        print("Copying output_annual.txt to output folder")
        shutil.copy(fn_biomass_file, output_folder)
        os.rename(os.path.join(output_folder, "annual.txt"), output_file_path)
        os.remove(fn_biomass_file)

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

    # Here is the key change: instead of "./output", we'll point to "/mnt/newvolume".
    parent_folders = ["future_c1_conv", "future_c2_conv", "future_c3_conv", "hist_conv"]
    
    for parent in parent_folders:
        # Keep the parent path for input data the same
        parent_path = f"./{parent}"

        # Change to desired output location
        output_parent_folder = os.path.join("/home/ec2-user/sasa_main_code/output", parent)
        if not os.path.exists(output_parent_folder):
            os.makedirs(output_parent_folder)

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

            # Build the path in the new output folder (/mnt/newvolume)
            # Note: file[2:] removes the leading "./" from the input file path
            output_file_path = os.path.join("/", file[2:])
            print("output_file_path:", output_file_path)

            output_folder = os.path.dirname(output_file_path)
            if os.path.exists(output_file_path):
                # If we already have the file, return the folder to the queue
                available_folders.put(folder)
                continue

            if not os.path.exists(output_folder):
                os.makedirs(output_folder)

            instance = len(processes)
            p = Process(target=run_a_exe, args=(instance, folder, available_folders, output_file_path))
            processes.append(p)
            p.start()

        for p in processes:
            p.join()

        processes.clear()

        print(f"All instances for {parent} have completed.")
