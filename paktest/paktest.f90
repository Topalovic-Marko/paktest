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

    integer :: status, file_unit, i
    logical :: file_exists, testing_complete
    character(len=100) :: command
    
    testing_complete = .false.
    call testiran(status, testing_complete)
    if (status /= 0) then
        write(*,*) 'Greska u prvom pozivu testiran sabrutine, unesi 1 da nastavis testiranje sledeceg primera'
        read(*,*) i
    end if

    do while (.not. testing_complete)
        inquire(file='pak.lst', exist=file_exists)
        if (file_exists) then
            open(unit=15, file='pak.lst', status='old')
            close(unit=15, status='delete')
        end if
        
        call execute_command_line('del z*', wait=.true.)
        call execute_command_line('del pak.unv', wait=.true.)
        call execute_command_line('del pak.neu', wait=.true.)

        command = 'test\pak < aa'
        call execute_command_line(command, wait=.true., exitstat=status)
        if (status /= 0) then
            write(*,*) 'Greska: PAK je negde crko, unesi 1 da nastavis testiranje sledeceg primera'
            read(*,*) i
        end if
        
        call testiran(status, testing_complete)
        if (status /= 0) then
            write(*,*) 'Greska u testiran sabrutini, unesi 1 da nastavis testiranje sledeceg primera'
            read(*,*) i
        end if
    end do

    inquire(file='pak.lst', exist=file_exists)
    if (file_exists) then
        open(unit=15, file='pak.lst', status='old')
        close(unit=15, status='delete')
    end if
    
    call execute_command_line('del z*', wait=.true.)
    call execute_command_line('del pak.unv', wait=.true.)
    call execute_command_line('del pak.neu', wait=.true.)

    end program paktest   

    subroutine testiran(status, testing_complete)
        
    IMPLICIT NONE

    ! Variable declarations
    CHARACTER(LEN=256) :: line, valueOfLine, search_str, compare_str, lst_str, lst_result, s2
    CHARACTER(LEN=1) :: char, emptySpace
    INTEGER :: i, numberOfLInes, m, n, current_hash_count, ios, test, red, startChar, endChar
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

    INQUIRE(FILE='pom.pom', EXIST=file_exists)
    IF (.NOT. file_exists) THEN
        OPEN(UNIT=UNIT_POM, FILE='pom.pom', STATUS='REPLACE')
        WRITE(UNIT_POM, *) 1, 1
        CLOSE(UNIT_POM)
    END IF

    lst_str = ''
    OPEN(UNIT=UNIT_POM, FILE='pom.pom', STATUS='OLD')
    READ(UNIT_POM, *) m, n
    CLOSE(UNIT_POM)

    INQUIRE(FILE='tabela.pod', EXIST=file_exists)
    IF (.NOT. file_exists) THEN
        status = 1
        RETURN
    END IF
    
    OPEN(UNIT=UNIT_POD, FILE='tabela.pod', STATUS='OLD', IOSTAT=ios)
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

            IF (current_hash_count == n) THEN

                OPEN(UNIT=UNIT_POM, FILE='pom.pom', STATUS='REPLACE')
                WRITE(UNIT_POM, *) m, n + 1
                CLOSE(UNIT_POM)
                
                PRINT *, 'proverava fajl ', TRIM(line(2:))

                OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                WRITE(UNIT_IZL, '(A)') TRIM(line(2:))
                CLOSE(UNIT_IZL)

                OPEN(UNIT=UNIT_AA, FILE='aa', STATUS='REPLACE')
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
            IF (current_hash_count == n-1) THEN
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

   

