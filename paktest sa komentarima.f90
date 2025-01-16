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
    
    ! Variables for system calls and file operations
    integer :: status, file_unit, i
    logical :: file_exists
    character(len=100) :: command
    
            call testiran(status)
        if (status /= 0) then
            write(*,*) 'Error: initial call to testiran subroutine failed'
            read(*,*) i
            !stop
        end if
    ! Main testing loop
    do
        call sleep(2)
        ! Clean up any existing output files
        call execute_command_line('del z*', wait=.true.)
        call execute_command_line('del pak.unv', wait=.true.)
        call execute_command_line('del pak.lst', wait=.true.)
        call execute_command_line('del pak.neu', wait=.true.)
        
        ! Run PAK solver with input file
        command = 'test\pak < aa'
        call execute_command_line(command, wait=.true., exitstat=status)
        if (status /= 0) then
            write(*,*) 'Error: PAK solver failed to execute, enter 1 to continue testing'
            read(*,*) i
            !stop
        end if
        
        ! Call testiran subroutine instead of external program
        call testiran(status)
        if (status /= 0) then
            write(*,*) 'Error: testiran subroutine failed, enter 1 to try next example'
            read(*,*) i
            !stop
        end if
        
        ! Check if success file exists
        inquire(file='zzz.zzz', exist=file_exists)
        
        ! If success file found, exit loop
        if (file_exists) then
            exit
        end if
    end do
    
    ! Final cleanup
    call execute_command_line('del z*', wait=.true.)
    call execute_command_line('del pak.unv', wait=.true.)
    call execute_command_line('del pak.lst', wait=.true.)
    call execute_command_line('del pak.neu', wait=.true.)
    
    write(*,*) 'Testing completed successfully'
    
contains
    subroutine testiran(status)
        
        
IMPLICIT NONE

    ! Variable declarations
    CHARACTER(LEN=256) :: line, valueOfLine, search_str, compare_str, lst_str, lst_result, s2
    CHARACTER(LEN=1) :: char, emptySpace
    INTEGER :: i,numberOfLInes, m, n, current_hash_count, ios ,test, red, startChar, endChar
    LOGICAL :: file_exists

    ! File units
    INTEGER, PARAMETER :: UNIT_AA = 10
    INTEGER, PARAMETER :: UNIT_IZL = 11
    INTEGER, PARAMETER :: UNIT_POD = 12
    INTEGER, PARAMETER :: UNIT_POM = 13
    INTEGER, PARAMETER :: UNIT_DAT = 14
    INTEGER, PARAMETER :: UNIT_LST = 15
    integer, intent(out) :: status
    
    ! Initialize pom.pom if it doesn't exist
    INQUIRE(FILE='pom.pom', EXIST=file_exists)
    IF (.NOT. file_exists) THEN
        OPEN(UNIT=UNIT_POM, FILE='pom.pom', STATUS='REPLACE')
        WRITE(UNIT_POM, *) 1, 1
        CLOSE(UNIT_POM)
    END IF
    lst_str = ''
    ! Read current values from pom.pom
    OPEN(UNIT=UNIT_POM, FILE='pom.pom', STATUS='OLD')
    READ(UNIT_POM, *) m, n
    CLOSE(UNIT_POM)

    OPEN(UNIT=UNIT_POD, FILE='tabela.pod', STATUS='OLD')
red = 0
    ! Process lines in tabela.pod
    current_hash_count = 0
    DO
        numberOfLInes = 0
        READ(UNIT_POD, '(A)', IOSTAT=ios) line
        IF (ios /= 0) then
            ! Create zzz.zzz to signal completion to the batch file
            OPEN(UNIT=UNIT_DAT, FILE='zzz.zzz', STATUS='REPLACE')
            CLOSE(UNIT_DAT)
            EXIT
        END IF
        
        ! pri svakom prolasku kroz petlju, pocinje da cita tabela.pod od pocetka i onda broji redove koji pocinju sa # dok ne dodje do aktuelnog primera sacuvanog u pom.pom kao n
        ! Check if line starts with #
        IF (line(1:1) == '#') THEN
            
            current_hash_count = current_hash_count + 1

            IF (current_hash_count == n) THEN ! znaci da je stigao do aktuelnog primera
                ! Update pom.pom to increment hash counter
                OPEN(UNIT=UNIT_POM, FILE='pom.pom', STATUS='REPLACE')
                WRITE(UNIT_POM, *) m, n + 1 ! ovo kaze da je naredni primer koji ce da se testira n+1 i to se zapisuje u pom.pom
                CLOSE(UNIT_POM)
                PRINT *, 'proverava fajl ', TRIM(line(2:))
                OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                !WRITE(UNIT_IZL, '(A,I0)') 'primer #',n
                WRITE(UNIT_IZL, '(A)') TRIM(line(2:))
                CLOSE(UNIT_IZL)
                !read(*,*) test
                ! Generate the aa  file
                OPEN(UNIT=UNIT_AA, FILE='aa.', STATUS='REPLACE') !ovo generise fajl aa. koji se koristi za pokretanje programa
                WRITE(UNIT_AA, '(A)') TRIM(line(2:))
                WRITE(UNIT_AA, '(A)') 'pak.lst'
                WRITE(UNIT_AA, '(A)') 'pak.unv'
                WRITE(UNIT_AA, '(A)') 'pak.neu'
                CLOSE(UNIT_AA)
                red = 1
                EXIT ! izlazi iz petlje jer je nasao aktuelni primer
            END IF
        else
            IF (current_hash_count == n-1) THEN ! znaci da je stigao do aktuelnog primera, posto je n sada vec buduci primer
            OPEN(UNIT=UNIT_LST, FILE='pak.lst', STATUS='OLD')
                IF ((line(2:2) == 't').OR.(line(2:2) == 'T')) THEN
! Handle T (TOP) command ! t je visak slepo crevo treba da bude top ali u tabela.pod nema top
                    READ(UNIT_POD, '(A1)', ADVANCE='NO') line
                    READ(UNIT_POD, '(A1)', ADVANCE='NO') line
                    REWIND(UNIT_LST)
                    red = 1
                end if
                IF ((line(2:2) == 'd').OR.(line(2:2) == 'D')) THEN
! Handle D (DOWN) command
                    OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                    !WRITE(UNIT_IZL, '(A)') line
                    valueOfLine = TRIM(line(6:))
                    read(valueOfLine, '(I)') numberOfLInes
                    DO i = 1, numberOfLInes
                        READ(UNIT_LST, *)
                    END DO
                    red = red + numberOfLInes
                CLOSE(UNIT_IZL)
                end if
                IF ((line(2:2) == 'c').OR.(line(2:2) == 'C')) THEN
! Handle C (COMP "str") command - string comparison
                OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                !WRITE(UNIT_IZL, '(A)') line
                valueOfLine = TRIM(line(6:))
                    !WRITE(UNIT_IZL, '(A)') valueOfLine
                    compare_str = valueOfLine(3:LEN_TRIM(valueOfLine)-1)
                    !WRITE(UNIT_IZL, '(A)') "string za poredjenje",compare_str
                    IF (lst_result == compare_str) THEN
                        WRITE(UNIT_IZL, '(A)') 'OK'
                    ELSE
                        WRITE(UNIT_IZL, '(A)') TRIM(lst_result) // '->' // TRIM(compare_str)
                    END IF
                CLOSE(UNIT_IZL)
                end if
                IF ((line(2:2) == 'r').OR.(line(2:2) == 'R')) THEN
! Handle R (READ num1 num2) command - read specific positions
                OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                !WRITE(UNIT_IZL, '(A)') line
                valueOfLine = TRIM(line(6:))
                !WRITE(UNIT_IZL, '(A)') valueOfLine
                    read(valueOfLine, *) startChar, endChar
                    !WRITE(UNIT_IZL, '(A,I)') "pocetni karakter", startChar
                    !WRITE(UNIT_IZL, '(A,I)') "krajnji karakter", endChar
                    READ(UNIT_LST, '(A)', IOSTAT=ios) lst_str
                    !WRITE(UNIT_IZL, '(A,A)') "ceo las rezultat:", lst_str
                    !WRITE(UNIT_IZL, '(A,I)') "cita iz linije:", red
                    lst_result = lst_str(startChar:endChar)
                    !WRITE(UNIT_IZL, '(A)') "lst rezultat",lst_result
                    REWIND(UNIT_LST)
                    DO i = 1, red
                        READ(UNIT_LST, *)
                    END DO
                    CLOSE(UNIT_IZL)
                end if
                IF ((line(2:2) == 'f').OR.(line(2:2) == 'F')) THEN
! Handle F (FIND "str") command - find string
                    OPEN(UNIT=UNIT_IZL, FILE='tabela.izl', POSITION='APPEND')
                    !WRITE(UNIT_IZL, '(A)') line
                    valueOfLine = TRIM(line(6:))
                    !WRITE(UNIT_IZL, '(A)') valueOfLine
                    search_str = valueOfLine(3:LEN_TRIM(valueOfLine)-1)
                    !WRITE(UNIT_IZL, '(A)') search_str
                    red = red - 1
                    DO ! Search for "search_str" string in LST file
                        READ(UNIT_LST, '(A)', IOSTAT=ios) s2
                        IF (ios /= 0) THEN
                            !WRITE(UNIT_IZL, '(A)') "dosao je do karaja lst fajla"
                            EXIT
                        END IF
                        red = red + 1
                        IF (INDEX(s2, TRIM(search_str)) > 0) THEN 
                            !WRITE(UNIT_IZL, '(A)') "nasao je string u lst"
                            EXIT
                        end IF
                    END DO
                    !
                    !! Return to found position
                    REWIND(UNIT_LST)
                    DO i = 1, red
                        READ(UNIT_LST, *)
                    END DO
                    CLOSE(UNIT_IZL)
                end if
  
            end if
            
        END IF
    END DO

    CLOSE(UNIT_POD)
        
    status = 0
        
    end subroutine testiran

    end program paktest

