import os
import time
import platform
import subprocess
import threading
from pathlib import Path

class PAKSTestRunner:
    def __init__(self, base_dir=None, max_wait_time=3):  # 5 minutes default max wait
        # Set base directory to script location if not provided
        self.base_dir = Path(base_dir or os.path.dirname(os.path.abspath(__file__)))
        
        self.is_windows = platform.system().lower() == 'windows'
        self.current_test = 1
        self.lst_result = ''
        self.max_wait_time = max_wait_time  # Maximum wait time in seconds
        
        # Verify and print important paths
        print(f"Base Directory: {self.base_dir}")
        print(f"Current Working Directory: {os.getcwd()}")
        
        # Check for required directories and files
        self._validate_environment()

    def _validate_environment(self):
        """Check for required directories and files"""
        # Test directory
        test_dir = self.base_dir / 'test'
        if not test_dir.exists():
            raise FileNotFoundError(f"Test directory not found: {test_dir}")
        
        # TABELA.POD file
        pod_file = test_dir / 'TABELA.POD'
        if not pod_file.exists():
            raise FileNotFoundError(f"TABELA.POD not found: {pod_file}")
        
        # PAKS executable
        paks_exe = 'PAKS.exe' if self.is_windows else './PAKS'
        paks_path = Path(paks_exe)
        if not paks_path.exists():
            raise FileNotFoundError(f"PAKS executable not found: {paks_exe}")

    def delete_if_exists(self, filename):
        """Delete file if it exists, with full path handling"""
        file_path = self.base_dir / filename
        try:
            if file_path.exists():
                file_path.unlink()
        except Exception as e:
            print(f"Error deleting {filename}: {e}")

    def wait_for_file(self, file_path):
        """More efficient file waiting with timeout"""
        start_time = time.monotonic()
        while time.monotonic() - start_time < self.max_wait_time:
            try:
                if file_path.is_file():
                    # Use less frequent checks and faster size comparison
                    with file_path.open('rb') as f:
                        f.seek(0, 2)  # Seek to end for fast size check
                        if f.tell() > 0:
                            return True
                time.sleep(0.1)  # Reduced sleep time
            except Exception:
                return False
        return False

    def run_test(self, test_file):
        """Run PAKS executable with input file and display real-time output"""
        paks_command = 'PAKS.exe' if self.is_windows else './PAKS'
        pak_lst_path = self.base_dir / 'pak.lst'
        
        try:
            # Change to base directory for execution
            original_dir = os.getcwd()
            os.chdir(self.base_dir)
            
            # Remove existing pak.lst if it exists
            if pak_lst_path.exists():
                pak_lst_path.unlink()
            
            # Start PAKS process with real-time output
            def print_output(pipe):
                for line in iter(pipe.readline, ''):
                    print(line.strip())  # Print each line as it's received
                pipe.close()
            
            # Start the process
            process = subprocess.Popen(
                [paks_command], 
                stdin=subprocess.PIPE, 
                stdout=subprocess.PIPE, 
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1,  # Line buffered
                universal_newlines=True
            )
            
            # Write input from 'aa' file
            input_path = self.base_dir / 'aa'
            input_content = input_path.read_text()
            process.stdin.write(input_content)
            process.stdin.close()
            
            # Start threads to read stdout and stderr
            import threading
            stdout_thread = threading.Thread(target=print_output, args=(process.stdout,))
            stderr_thread = threading.Thread(target=print_output, args=(process.stderr,))
            
            # Start the threads
            stdout_thread.start()
            stderr_thread.start()
            
            # Wait for process to complete
            process.wait()
            
            # Wait for output threads to finish
            stdout_thread.join()
            stderr_thread.join()
            
            # Check process return code
            if process.returncode != 0:
                print(f"PAKS process failed with return code {process.returncode}")
                return 1
            
            # Wait for pak.lst to be created and fully written
            if not self.wait_for_file(pak_lst_path):
                print("Error: pak.lst was not created or is incomplete")
                return 1
            
            return 0
        
        except Exception as e:
            print(f"Unexpected error running PAKS: {e}")
            return 1
        finally:
            # Ensure we change back to original directory
            os.chdir(original_dir)

    def testiran(self):
        """Main testing function that processes TABELA.POD and compares results"""
        pod_file_path = self.base_dir / 'test' / 'TABELA.POD'
        
        try:
            with open(pod_file_path, 'r') as pod_file, \
                 open(self.base_dir / 'tabela.izl', 'a') as izl_file:
                current_hash_count = 0
                red = 0
                lst_lines = []
                
                for line in pod_file:
                    line = line.strip()
                    if not line:
                        continue
                        
                    if line.startswith('#'):
                        current_hash_count += 1
                        
                        if current_hash_count == self.current_test:
                            print(f'Checking file {line[1:].strip()}')
                            
                            # Append to tabela.izl
                            izl_file.write(f'{line[1:].strip()}\n')
                            
                            # Create aa file
                            test_example = line[1:].strip()
                            if not self.is_windows:
                                test_example = test_example.replace('\\', '/')
                                if test_example.endswith('.'):
                                    test_example = test_example[:-1]
                                    
                            with open(self.base_dir / 'aa', 'w') as aa_file:
                                aa_file.write(f'{test_example}\n')
                                aa_file.write('pak.lst\n')
                                aa_file.write('pak.unv\n')
                                aa_file.write('pak.neu\n')
                                
                            return 0, False  # Continue testing
                            
                    else:
                        if current_hash_count == self.current_test - 1:
                            # Check if pak.lst exists
                            pak_lst_path = self.base_dir / 'pak.lst'
                            
                            # Wait for pak.lst to be created and fully written
                            if not self.wait_for_file(pak_lst_path):
                                print("Error: pak.lst was not created or is incomplete")
                                return 1
                            #if not pak_lst_path.exists():
                            #    print("pak.lst not found")
                            #    return 3, False
                            
                            # Read lst file lines
                            with open(pak_lst_path, 'r') as lst_file:
                                lst_lines = lst_file.readlines()
                            # Process different command types
                            command_type = line[0:1].lower()
                            if command_type == 't':
                                # Text or initial processing
                                red = 1
                            elif command_type == 'f':
                                # Find line containing a specific string
                                try:
                                    search_str = line[5:].strip()[1:-1]
                                    red -= 1
                                    
                                    # Search for line containing the string
                                    for i in range(red, len(lst_lines)):
                                        red += 1
                                        if search_str in lst_lines[i]:
                                            break
                                except IndexError:
                                    print(f"Error processing 'f' command: {line}")
                                    
                            elif command_type == 'd':
                                # Delete or skip specified number of lines
                                try:
                                    num_lines = int(line[5:].strip())
                                    # Skip lines and update red
                                    red += num_lines
                                except (ValueError, IndexError):
                                    print(f"Error processing 'd' command: {line}")
                                    
                            elif command_type == 'r':
                                # Read specific characters from a line
                                try:
                                    # Parse start and end characters
                                    start_char, end_char = map(int, line[5:].strip().split())
                                    # Check if there are enough lines
                                    if red < len(lst_lines):
                                        # Extract substring from specified line and range
                                        self.lst_result = lst_lines[red-1][start_char-1:end_char]
                                except (ValueError, IndexError):
                                    print(f"Error processing 'r' command: {line}")
                            
                            elif command_type == 'c':
                                # Comparison of last result with expected value
                                try:
                                    # Remove quotes and strip whitespace
                                    compare_str = line[5:].strip()[1:-1].strip()
                                    current_result = self.lst_result.strip()
                                    
                                    # Try to parse as floating-point numbers for scientific notation comparison
                                    try:
                                        # Normalize scientific notation
                                        compare_float = float(compare_str.replace('D', 'E'))
                                        result_float = float(current_result.replace('D', 'E'))
                                        
                                        # Use approximate comparison with a small tolerance
                                        if abs(compare_float - result_float) < 1e-10:
                                            izl_file.write('OK\n')
                                        else:
                                            izl_file.write(f'{current_result}->{compare_str}\n')
                                    
                                    except ValueError:
                                        # Fallback to string comparison if not numeric
                                        if current_result == compare_str:
                                            izl_file.write('OK\n')
                                        else:
                                            izl_file.write(f'{current_result}->{compare_str}\n')
                                
                                except IndexError:
                                    print(f"Error processing 'c' command: {line}")

        
        except FileNotFoundError:
            print(f"Error: TABELA.POD not found at {pod_file_path}")
            return 2, True
        except Exception as e:
            print(f"Error in testiran: {e}")
            return 2, True
            
        return 0, True

    def cleanup_files(self):
        """Clean up all temporary files"""
        files_to_delete = [
            'pak.unv', 'pak.neu',
            'aa', 'CONTROL.SRE', 'dijagram', 'EGGOS', 'VREMENA',
            'GE0001.neu', 'SE0001', 'POMER1', 'pak.neu'
        ]
        
        for file in files_to_delete:
            self.delete_if_exists(file)
    
    def delete_files_prefix(self, prefix):

        is_windows = platform.system().lower() == 'windows'
        
        try:
            # Construct delete command based on OS
            if is_windows:
                command = f'del {prefix}* /Q'
            else:
                command = f'rm -f {prefix}*'
            
            # Execute the command
            result = subprocess.run(
                command, 
                shell=True, 
                stdout=subprocess.PIPE, 
                stderr=subprocess.PIPE,
                text=True
            )
            
            # Return exit status (0 for success)
            return result.returncode
        
        except Exception as e:
            print(f"Error deleting files with prefix {prefix}: {e}")
            return 1 


    def delete_files_suffix(self, suffix):  
        
        is_windows = platform.system().lower() == 'windows'

        try:
            # Construct the delete command based on OS
            if is_windows:
                command = f'del /F /Q *{suffix}'
            else:
                # Use `shlex` to properly handle wildcard expansion
                command = f'rm -f -- *{suffix}'

            # Execute the command in a way that expands wildcards on Linux
            result = subprocess.run(
                command, 
                shell=True, 
                stdout=subprocess.PIPE, 
                stderr=subprocess.PIPE,
                text=True,
                executable="/bin/bash" if not is_windows else None  # Use bash for wildcard expansion
            )

            if result.returncode != 0:
                print(f"Error: {result.stderr}")

            return result.returncode

        except Exception as e:
            print(f"Error deleting files with suffix {suffix}: {e}")
            return 1


def main():
    try:
        runner = PAKSTestRunner()
        
        # Initial cleanup
        runner.delete_if_exists('tabela.izl')
        
        # Main testing loop
        testing_complete = False
        while not testing_complete:
            # Clean up before each test
            #runner.delete_if_exists('pak.lst')
            runner.delete_if_exists('pak.unv')
            runner.delete_if_exists('pak.neu')
            
            # Run test
            status, testing_complete = runner.testiran()
            if status != 0:
                input('Error in testiran subroutine, press 1 to continue testing next example: ')
                
            # Run PAKS
            if not testing_complete:
                status = runner.run_test('aa')
                if status != 0:
                    input('Error: PAK crashed somewhere, press 1 to continue testing next example: ')
                
                runner.current_test += 1
        
        # Final cleanup
        runner.cleanup_files()
        runner.delete_files_prefix('Z')
        runner.delete_files_prefix('fort')
        runner.delete_files_suffix('.UNV')
        runner.delete_files_suffix('.CSV')
        runner.delete_files_suffix('.lst')
        
    except Exception as e:
        print(f"Fatal error: {e}")

if __name__ == "__main__":
    main()
