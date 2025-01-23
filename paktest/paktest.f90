!  paktest.f90 
!
!  FUNCTIONS:
!  paktest - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: paktest
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program paktest

    implicit none

    integer :: status, file_unit, i, current_test = 1  ! Added current_test to track test number
    INTEGER, PARAMETER :: UNIT_LST = 15
    INTEGER, PARAMETER :: UNIT_DEL = 42
    logical :: file_exists, testing_complete
    logical :: is_windows
    character(len=100) :: command
    
    is_windows = .false.
    testing_complete = .false.
    call detect_os(is_windows)

    call delete_if_exists('tabela.izl',UNIT_DEL)
    
    call testiran(status, testing_complete, current_test,is_windows)
    if (status /= 0) then
        write(*,*) 'Greska u prvom pozivu testiran sabrutine, unesi 1 da nastavis testiranje sledeceg primera'
        read(*,*) i
    end if

    do while (.not. testing_complete)

        call delete_if_exists('pak.lst',UNIT_LST)
        call delete_if_exists('pak.unv',UNIT_DEL)
        call delete_if_exists('pak.neu',UNIT_DEL)
        call delete_files_prefix('Z',is_windows)

        if (is_windows) then      ! Windows version
            command = 'PAKS < aa'
        else        ! Linux/Unix version
            command = './PAKS < aa'
        endif
        !command = 'test\pak < aa'
        
        call execute_command_line(command, wait=.true., exitstat=status)
        if (status /= 0) then
            write(*,*) 'Greska: PAK je negde crko, unesi 1 da nastavis testiranje sledeceg primera'
            read(*,*) i
        end if
        
        current_test = current_test + 1  ! Increment test counter
        call testiran(status, testing_complete, current_test,is_windows)
        if (status /= 0) then
            write(*,*) 'Greska u testiran sabrutini, unesi 1 da nastavis testiranje sledeceg primera'
            read(*,*) i
        end if
    end do

    call delete_if_exists('pak.lst',UNIT_LST)
    call delete_if_exists('pak.unv',UNIT_DEL)
    call delete_if_exists('pak.neu',UNIT_DEL)
    call delete_files_prefix('Z',is_windows)
    
    call delete_if_exists('aa',UNIT_DEL)
    call delete_if_exists('CONTROL.SRE',UNIT_DEL)
    call delete_if_exists('dijagram',UNIT_DEL)
    call delete_if_exists('EGGOS',UNIT_DEL)
    call delete_if_exists('GE0001.neu',UNIT_DEL)
    call delete_if_exists('SE0001',UNIT_DEL)
    call delete_if_exists('POMER1',UNIT_DEL)
    call delete_if_exists('pak.neu',UNIT_DEL)
    
    call delete_files_prefix('fort.',is_windows)
    call delete_files_sufix('.UNV',is_windows)
    
    end program paktest   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine testiran(status, testing_complete, current_test,is_windows)
        
    IMPLICIT NONE

    !Variable declarations
    CHARACTER(LEN=256) :: line, valueOfLine, search_str, compare_str, lst_str, lst_result, s2
    CHARACTER(LEN=1) :: char, emptySpace
    character(len=100) :: command, test_example
    INTEGER :: i, numberOfLInes, current_hash_count, ios, test, red, startChar, endChar
    INTEGER, intent(in) :: current_test  ! Added current_test as input parameter
    logical, intent(in) :: is_windows
    LOGICAL :: file_exists
    LOGICAL, intent(inout) :: testing_complete
    
    ! File units
    INTEGER, PARAMETER :: UNIT_AA = 10
    INTEGER, PARAMETER :: UNIT_IZL = 11
    INTEGER, PARAMETER :: UNIT_POD = 12
    INTEGER, PARAMETER :: UNIT_POM = 13
    INTEGER, PARAMETER :: UNIT_DAT = 14
    INTEGER, PARAMETER :: UNIT_LST = 15
    integer, intent(out) :: status

    status = 0

    lst_str = ''

    INQUIRE(FILE='test/tabela.pod', EXIST=file_exists)
    IF (.NOT. file_exists) THEN
        status = 1
        RETURN
    END IF
    
    OPEN(UNIT=UNIT_POD, FILE='test/tabela.pod', STATUS='OLD', IOSTAT=ios)
    IF (ios /= 0) THEN
        status = 2
        RETURN
    END IF

    red = 0
    current_hash_count = 0
    DO
        numberOfLInes = 0
        READ(UNIT_POD, '(A)', IOSTAT=ios) line
        IF (ios /= 0) THEN
            testing_complete = .true.
            CLOSE(UNIT_POD)
            EXIT
        END IF

        IF (line(1:1) == '#') THEN
            current_hash_count = current_hash_count + 1

            IF (current_hash_count == current_test) THEN
                PRINT *, 'proverava fajl ', TRIM(line(2:))

                OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                WRITE(UNIT_IZL, '(A)') TRIM(line(2:))
                CLOSE(UNIT_IZL)

                OPEN(UNIT=UNIT_AA, FILE='aa', STATUS='REPLACE')
                if (is_windows) then      ! Windows version
                    test_example = TRIM(line(2:))
                else        ! Linux/Unix version
                    test_example = TRIM(line(2:))
                endif
                WRITE(UNIT_AA, '(A)') TRIM(line(2:))
                WRITE(UNIT_AA, '(A)') 'pak.lst'
                WRITE(UNIT_AA, '(A)') 'pak.unv'
                WRITE(UNIT_AA, '(A)') 'pak.neu'
                CLOSE(UNIT_AA)
                
                red = 1
                CLOSE(UNIT_POD)
                EXIT
            END IF
        ELSE
            IF (current_hash_count == current_test-1) THEN
                INQUIRE(FILE='pak.lst', EXIST=file_exists)
                IF (.NOT. file_exists) THEN
                    CLOSE(UNIT_POD)
                    status = 3
                    RETURN
                END IF
                
                OPEN(UNIT=UNIT_LST, FILE='pak.lst', STATUS='OLD', IOSTAT=ios)
                IF (ios /= 0) THEN
                    CLOSE(UNIT_POD)
                    status = 4
                    RETURN
                END IF

                IF ((line(2:2) == 't').OR.(line(2:2) == 'T')) THEN
                    READ(UNIT_POD, '(A1)', ADVANCE='NO') line
                    READ(UNIT_POD, '(A1)', ADVANCE='NO') line
                    REWIND(UNIT_LST)
                    red = 1
                END IF

                IF ((line(2:2) == 'd').OR.(line(2:2) == 'D')) THEN
                    OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                    valueOfLine = TRIM(line(6:))
                    READ(valueOfLine, '(I)', IOSTAT=ios) numberOfLInes
                    IF (ios == 0) THEN
                        DO i = 1, numberOfLInes
                            READ(UNIT_LST, *, IOSTAT=ios)
                            IF (ios /= 0) EXIT
                        END DO
                        red = red + numberOfLInes
                    END IF
                    CLOSE(UNIT_IZL)
                END IF

                IF ((line(2:2) == 'c').OR.(line(2:2) == 'C')) THEN
                    OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                    valueOfLine = TRIM(line(6:))
                    compare_str = valueOfLine(3:LEN_TRIM(valueOfLine)-1)
                    IF (lst_result == compare_str) THEN
                        WRITE(UNIT_IZL, '(A)') 'OK'
                    ELSE
                        WRITE(UNIT_IZL, '(A)') TRIM(lst_result) // '->' // TRIM(compare_str)
                    END IF
                    CLOSE(UNIT_IZL)
                END IF

                IF ((line(2:2) == 'r').OR.(line(2:2) == 'R')) THEN
                    OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                    valueOfLine = TRIM(line(6:))
                    READ(valueOfLine, *, IOSTAT=ios) startChar, endChar
                    IF (ios == 0) THEN
                        READ(UNIT_LST, '(A)', IOSTAT=ios) lst_str
                        IF (ios == 0) THEN
                            lst_result = lst_str(startChar:endChar)
                        END IF
                    END IF
                    REWIND(UNIT_LST)
                    DO i = 1, red
                        READ(UNIT_LST, *, IOSTAT=ios)
                        IF (ios /= 0) EXIT
                    END DO
                    CLOSE(UNIT_IZL)
                END IF

                IF ((line(2:2) == 'f').OR.(line(2:2) == 'F')) THEN
                    OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                    valueOfLine = TRIM(line(6:))
                    search_str = valueOfLine(3:LEN_TRIM(valueOfLine)-1)
                    red = red - 1
                    DO 
                        READ(UNIT_LST, '(A)', IOSTAT=ios) s2
                        IF (ios /= 0) EXIT
                        red = red + 1
                        IF (INDEX(s2, TRIM(search_str)) > 0) EXIT
                    END DO

                    REWIND(UNIT_LST)
                    DO i = 1, red
                        READ(UNIT_LST, *, IOSTAT=ios)
                        IF (ios /= 0) EXIT
                    END DO
                    CLOSE(UNIT_IZL)
                END IF
            END IF
        END IF
    END DO
        
    end subroutine testiran
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    subroutine delete_if_exists(filename, UNIT_DEL)
        character(len=*), intent(in) :: filename
        INTEGER, intent(in) :: UNIT_DEL
        logical :: exists
        integer :: ios
        inquire(file=filename, exist=exists)
        if (exists) then
            open(UNIT=UNIT_DEL, file=filename, status='old')!, iostat=ios)
            !if (ios == 0) then
                close(UNIT_DEL, status='delete', iostat=ios)
            !end if
        end if
    end subroutine delete_if_exists
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    subroutine delete_files_prefix(prefix,is_windows)
    character(len=*), intent(in) :: prefix
    logical, intent(in) :: is_windows
    integer :: status
    character(len=100) :: command
    call detect_os(is_windows)
    if (is_windows) then      ! Windows version
        command = 'del ' // trim(prefix) // '* /Q'
    else        ! Linux/Unix version
        command = 'rm -f ' // trim(prefix) // '*'
    endif
    call execute_command_line(command, wait=.true., exitstat=status)
    end subroutine delete_files_prefix
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    subroutine delete_files_sufix(sufix,is_windows)
    character(len=*), intent(in) :: sufix
    logical, intent(in) :: is_windows
    integer :: status
    character(len=100) :: command
    call detect_os(is_windows)
    if (is_windows) then      ! Windows version
        command = 'del *' // trim(sufix) // ' /Q'
    else        ! Linux/Unix version
        command = 'rm -f *' // trim(sufix)
    endif
    call execute_command_line(command, wait=.true., exitstat=status)
    end subroutine delete_files_sufix
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    subroutine detect_os(is_windows)
    logical, intent(out) :: is_windows
    character(256) :: os_name, windir
    integer :: status
    ! Try Windows-specific environment variable
    call get_environment_variable('WINDIR', windir, status=status)
    if (status == 0) then
        is_windows = .true.
        write(*,*) 'Detected Windows'
        return
    end if
    ! If we get here, assume it's Unix/Linux
    is_windows = .false.
    end subroutine detect_os
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!