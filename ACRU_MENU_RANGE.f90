!###################################################################
! TITLE        : ACRU_SUB_MENU_GENERATOR
!-------------------------------------------------------------------
! EDITED BY    : Dr. Stefan W. Kienzle
! DATE EDITED  : Otober 9, 2009
! REVISED BY   : Charmaine Bonifacio
! DATE REVISED : December 14, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The program will read a MENU file and selects the
!                new range of HRU based on the min and max HRU #.
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! INPUT        : 1) Lowest HRU Number
!                2) Highest HRU Number
!                3) Total HRU Number
! OUTPUT       : 1) New MENU File
!                2) Logfile
!                3) Old MENU File
!###################################################################
program acru_menu_range
implicit none

    character(11), parameter :: debugstat = '[ STATUS ] '
    character(11), parameter :: debugres = '[ RESULT ] '
    character(11), parameter :: debugask = '[  ASK  ] '
    character(20), parameter :: dayStat = '             DATE : '
    character(20), parameter :: timeStat = '             TIME : '
    character(20), parameter :: etimeStat = '     ELAPSED TIME : '
    character(20), parameter :: logfileStat = '          LOGFILE : '
    character(20), parameter :: fileNameOpened =  '  FILENAME OPENED : '
    character(20), parameter :: fileStat =  '      FILE STATUS : '
    character(len=*), parameter:: format_hrufirst = '( 1X,A10,A35,I4 )'
    character(len=*), parameter:: format_hrulast = '( 1X,A10,A34,I4 )'
    character(len=*), parameter:: format_hrunum = '( 1X,A10,A36,I4 )'
    character(len=*), parameter:: format_isubnoline = '( 3(3X,I0.4),6X,I1 )'
    character(len=*), parameter:: format_isubno = '( 3X,I4 )'
    character(len=*), parameter :: format_etime = '( 1X,A11,A20,F10.5 )'
    character(len=*), parameter :: format_logfile = '( 1X,A11,A20,A31 )'
    character(len=*), parameter :: format_logstat = '( 1X,A11,A20,A20 )'
    character(len=*), parameter :: format_daytime = '( 1X,A11,A20,A15 )'
    character(len=*), parameter :: format_filestat = '( 1X,A11,A20,I4 )'
    character(len=*), parameter :: format_endmsg = '( A77,A10,A2,A5,A1 )'
    character(len=*), parameter :: msg = 'ACRU SUB MENU SCRIPT CREATED BY CHARMAINE BONIFACIO. VERSION DECEMBER 2015. ['
    integer :: ok, line_num
    integer :: hrunum, hrufirst, hrulast, minsub, maxsub, loopbk = 0
    integer :: isubno, isubnoline
    integer :: count_0, count_1, count_rate, count_max, counter, count2
    character(4), parameter :: menu = 'MENU'
    character(len=200) :: outfile, infile, logrun
    character(len=129) :: line
    character(len=8) :: dateinfo
    character(len=4) :: year, month*2, day*2
    character(len=2) :: hrs, min, sec*6
    character(len=10) :: date, timeinfo, datenow, dateend
	character(len=12) :: timenow, timeend
    logical :: ex

!***********************************************************************
! setup new menu file
!***********************************************************************
      call date_and_time(dateinfo, timeinfo)
      call system_clock(count_0, count_rate, count_max)
      year = dateinfo(1:4)
      month = dateinfo(5:6)
      day = dateinfo(7:8)
      date = year // '_' // month // '_' // day
      datenow = year // '-' // month // '-' // day
      hrs = timeinfo(1:2)
      min = timeinfo(3:4)
      sec = timeinfo(5:10)
      timenow = hrs // ':' // min // ':' // sec
!***********************************************************************
! start program
!***********************************************************************
      write(*,*)
      write(*,*) "###################################################################"
      write(*,*) ' '
      write(*,*) '     THE ACRU_MENU PROGRAM WILL CREATE A REVISED MENU FILE WITH  '
      write(*,*) '     A SUBSET OF THE ORIGINAL NUMBER OF HRUS. '
      write(*,*) ' '
      write(*,*) "###################################################################"
      write(*,*)
!***********************************************************************
! date
!***********************************************************************
      logrun = 'LOGRUN_MENU_'//date//'.txt'
      inquire(file=logrun, exist=ex)
      write(*,*) debugstat, ' checking file: ', logrun
      if (ex) then
        open(unit=12,file=logrun,status='replace',iostat=ok)
      else
        open(unit=12,file=logrun,status='new',iostat=ok)
      endif
      if (ok/=0) then
        write(*,*) debugres, 'COULD NOT OPEN FILE.'
        stop
      endif
      write(12,*)
!***********************************************************************
! start log
!***********************************************************************
      write(12,*) 'START OF PROGRAM. '
      write(12,*)
      write(12,*) "###################################################################"
      write(12,*) ' '
      write(12,*) '     THE ACRU_MENU PROGRAM WILL CREATE A REVISED MENU FILE WITH '
      write(12,*) '     A SUBSET OF THE ORIGINAL NUMBER OF HRUS. '
      write(12,*) ' '
      write(12,*) "###################################################################"
      write(12,*)
	  write(12,format_daytime) debugstat, dayStat, datenow
      write(12,format_daytime) debugstat, timeStat, timenow
      write(12,*)
      write(12,format_logfile ) debugstat, logfileStat, logrun
      write(12,format_filestat ) debugstat, fileStat  , ok
      infile = menu
	  open(unit=20,file=infile,iostat=ok)
      write(12,format_logstat) debugstat, fileNameOpened , infile
      write(12,format_filestat ) debugstat, fileStat, ok
      outfile = 'SUB_'//menu
      open(unit=30,file=outfile,iostat=ok)
      write(12,format_logstat) debugstat, fileNameOpened , outfile
      write(12,format_filestat ) debugstat,  fileStat , ok
      write(12,*)
!***********************************************************************
! start user input
!***********************************************************************
      write(*,*) " ENTER THE FIRST HRU NUMBER TO PROCESS : "
      read(*,*) hrufirst
      write(12,format_hrufirst) debugask, " THE FIRST HRU NUMBER TO PROCESS : ", hrufirst
!***********************************************************************
! continue user input
!***********************************************************************
      write(*,*) " ENTER THE LAST HRU NUMBER TO PROCESS : "
      read(*,*) hrulast
      write(12,format_hrulast) debugask, " THE LAST HRU NUMBER TO PROCESS : ", hrulast
!***********************************************************************
! continue user input
!***********************************************************************
      write(*,*) " ENTER THE TOTAL HRU NUMBER IN THE MENU : "
      read(*,*) hrunum
      write(12,format_hrunum) debugask, " THE TOTAL HRU NUMBER IN THE MENU : ", hrunum
      write(12,*)
!***********************************************************************
! check values first
!***********************************************************************
	  counter = 0
      do 700 while (counter.lt.12)
	     counter = counter + 1
         if (counter == 11) then
            read(20,format_isubno) isubnoline
         else
            read(20,100) line
         end if
  700 end do
      close(20)
      if (isubnoline == hrunum) then
         write(12,*) debugstat, "ISUBNO value checked."
         isubno = (hrulast + 1) - hrufirst
         line_num = (23 + (isubno + 5) * 150) + isubno
      else
         write(12,*) debugstat, "ISUBNO value invalid."
         isubno = 0
      end if
      if (hrufirst < hrulast .and. hrufirst < hrunum) then
         write(12,*) debugstat, "MINSUB value checked."
         minsub = 1
      else
         write(12,*) debugstat, "MINSUB value invalid."
		 minsub = 0
      endif
      if (hrufirst < hrulast .and. hrulast <= hrunum) then
         write(12,*) debugstat, "MAXSUB value checked."
         maxsub = isubno
      else
         write(12,*) debugstat, "MAXSUB value invalid."
		 maxsub = 0
      endif
      write(12,*)
      write(12,*) debugstat, "ISUBNO value in menu file: ", isubnoline
      write(12,*) debugstat, "ISUBNO value in sub menu file: ", isubno
      write(12,*) debugstat, "NEW TOTAL NUMBER OF LINE IN THE MENU : ", line_num
      write(12,*)
      if (minsub == 0 .or. maxsub == 0 .or. isubno == 0) then
         write(12,*) '*****************************************************************'
         write(12,*)
		 write(12,*) debugstat, "Incorrect HRU values were entered. Please try again."
		 write(12,*) debugstat, "Exiting program."
         write(12,*)
         write(12,*) '*****************************************************************'
         write(12,*)
         write(12,*) 'END OF PROGRAM. '
         close(30, status='delete')! only keep log and menu file
         close(20)
         close(12)
         stop
      endif
!***********************************************************************
! start processing file
!***********************************************************************
      open(unit=20,file=infile,iostat=ok)
      count2 = 1
	  counter = 0
  100 format(a80)
!     copy first 17 lines
      do 701 while (counter.lt.18)
	     counter = counter + 1
         read(20,100,end=999)line
         if (counter == 11) then
           write(30,format_isubnoline) isubno, minsub, maxsub, loopbk
         else
           write(30,100)line
         endif
  701 continue
!     proceed with the rest of the menu file but write only selected hrus
      do 800 while (count2 < 147) ! CONTAINERS
         counter = 0
         read(20,100,end=999)line
         write(30,100)line
         read(20,100,end=999)line
         write(30,100)line
         read(20,100,end=999)line
         write(30,100)line
         read(20,100,end=999)line
         write(30,100)line
         read(20,100,end=999)line
         do 801 while (counter.lt.hrunum)
            counter = counter + 1
            if(hrufirst <= counter .and. hrulast >= counter ) then
              write(30,100)line
            endif
            read(20,100,end=999)line
  801    continue
         if (count2 == 146) then
      		write(30,format_endmsg) msg, date, '//', timenow,']'
      		endfile(30)
         else
            write(30,100)
         end if
         count2 = count2 + 1
  800 continue
      close(30)
      close(20)
  999 write(*,*) '****************************************************'

!***********************************************************************
! elapsed time
!***********************************************************************
      call date_and_time(dateinfo, timeinfo)
      call system_clock(count_1, count_rate, count_max)
      year = dateinfo(1:4)
      month = dateinfo(5:6)
      day = dateinfo(7:8)
      date = year // '_' // month // '_' // day
      dateend = year // '-' // month // '-' // day
      hrs = timeinfo(1:2)
      min = timeinfo(3:4)
      sec = timeinfo(5:10)
      timeend = hrs // ':' // min // ':' // sec
!***********************************************************************
! end program
!***********************************************************************
      write(*,*) " Check logfile for more information."
      write(*,*)
	  write(12,*) "###################################################################"
      write(12,*) ' '
      write(12,*) '   THE ACRU_MENU PROGRAM HAS FINISHED CREATING A NEW MENU FILE. '
      write(12,*) ' '
      write(12,*) "###################################################################"
      write(12,*)
      write(12,format_daytime) debugstat, dayStat, dateend
      write(12,format_daytime) debugstat, timeStat, timeend
      write(12,*)
      write(12,format_etime ) debugstat, etimeStat, real(count_1 - count_0)/ real(count_rate)
      write(12,*)
      write(12,*) 'END OF PROGRAM. '
      close(12)
   	  stop
end program acru_menu_range
