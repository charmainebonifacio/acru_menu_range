!###################################################################
! TITLE        : ACRU_MENU_RANGE
!-------------------------------------------------------------------
! EDITED BY    : Dr. Stefan W. Kienzle
! DATE EDITED  : Otober 9, 2009
! REVISED BY   : Charmaine Bonifacio
! DATE REVISED : December 5, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The program will read a MENU file and selects the
!                new range of HRU based on the min and max HRU #.
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! INPUT        : 1) Lowest HRU Number
!                2) Highest HRU Number
! OUTPUT       : 1) New MENU File
!###################################################################
program acru_menu_range
implicit none

    character(len=11), parameter :: debugstat = '[ STATUS ] '
    character(len=11), parameter :: debugres = '[ RESULT ] '
    character(len=11), parameter :: debugask = '[  ASK  ] '
    character(len=*), parameter :: format_processed = '( 1X,A11,I7,A53 )'
    character(len=*), parameter:: format_hrufirst = '( A35,I4 )'
    character(len=*), parameter:: format_hrulast = '( A34,I4 )'
    character(len=*), parameter:: format_hrunum = '( A36,I4 )'
    character(len=*), parameter:: format_isubno = '( 3(3X,I0.4),6X,I1 )'
    character(len=*), parameter :: format_endmsg = '( A73,A10,A2,A5,A1 )'
    character(len=*), parameter :: msg = 'ACRU MENU SCRIPT REVISED BY CHARMAINE BONIFACIO. VERSION DECEMBER 2015. ['
    character(len=*), parameter :: lines_processed_msg = ' NUMBER OF PROCESSED LINES IN THE MENU PARAMETER FILE.'
    integer :: ok
    integer :: hrunum, hrufirst, hrulast, isubno, minsub, maxsub, loopbk = 0
    integer :: count_0, count_1, count_rate, count_max, counter, count2
    character(len=4), parameter :: menu = 'MENU'
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
	  write(12,*) debugstat, ' DATE -> ', datenow
      write(12,*) debugstat, ' TIME -> ', timenow
      write(12,*)
      write(12,*) debugstat, ' LOGFILE -> ', logrun
      write(12,*) debugstat, ' STATUS -> ', ok
      infile = menu
	  open(unit=20,file=infile,iostat=ok)
      write(12,*) debugstat, ' MENUFILE -> ', infile
      write(12,*) debugstat, ' STATUS -> ', ok
      outfile = menu//'_SELECTED_HRU'
      open(unit=30,file=outfile,iostat=ok)
      write(12,*) debugstat, ' MENUFILE COPY -> ', outfile
      write(12,*) debugstat, ' STATUS -> ', ok
      write(12,*)
!***********************************************************************
! start user input
!***********************************************************************
      write(*,*) " ENTER THE FIRST HRU NUMBER TO PROCESS : "
      read(*,*) hrufirst
      write(12,format_hrufirst) " THE FIRST HRU NUMBER TO PROCESS : ", hrufirst
!***********************************************************************
! continue user input
!***********************************************************************
      write(*,*) " ENTER THE LAST HRU NUMBER TO PROCESS : "
      read(*,*) hrulast
      write(12,format_hrulast) " THE LAST HRU NUMBER TO PROCESS : ", hrulast
!***********************************************************************
! continue user input
!***********************************************************************
      write(*,*) " ENTER THE TOTAL HRU NUMBER IN THE MENU : "
      read(*,*) hrunum
      write(12,format_hrunum) " THE TOTAL HRU NUMBER IN THE MENU : ", hrunum
      write(12,*)
!***********************************************************************
! start processing file
!***********************************************************************
      count2 = 1
	  counter = 0
      ! Check values first
      if (hrufirst < hrulast .and. hrufirst < hrunum) then
         write(12,*) debugstat, "MINSUB value checked."
         minsub = hrufirst
      endif
      if (hrufirst < hrulast .and. hrulast <= hrunum) then
         write(12,*) debugstat, "MAXSUB value checked."
         maxsub = hrulast
      endif
      isubno = (hrulast + 1) - hrufirst
      if (isubno <= hrunum) then
         write(12,*) debugstat, "ISUBNO value checked."
         write(12,*)
      end if
  100 format(a80)
!     copy first 18 lines
      do 700 while (counter.lt.17)
	     counter = counter + 1
         read(20,100,end=999)line
         if (counter == 11) then
           write(30,format_isubno) isubno, minsub, maxsub, loopbk
         else
           write(30,100)line
         endif
  700 continue
!     proceed with the rest of the menu file but write only selected hrus
      do 800 while (count2.lt.147)
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
            read(20,100,end=999)line
            if(counter.ge.hrufirst.and.counter.le.hrulast) then
              write(30,100)line
            endif
  801    continue
         count2 = count2 + 1
  800 continue
      write(*,*)
  999 write(*,*) '****************************************************'
      close(20)
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
      write(*,*)
      write(*,format_processed) debugstat, counter, lines_processed_msg
      write(12,*) "###################################################################"
      write(12,*) ' '
      write(12,*) '   THE ACRU_MENU PROGRAM HAS FINISHED CREATING A NEW MENU FILE. '
      write(12,*) ' '
      write(12,*) "###################################################################"
      write(12,*)
      write(12,format_processed) debugstat, counter, lines_processed_msg
      write(12,*)
      write(12,*) debugstat, ' DATE  -> ', dateend
      write(12,*) debugstat, ' TIME  -> ', timeend
      write(12,*)
      write(12,*) debugstat, ' ELAPSED TIME : ', real(count_1 - count_0)/ real(count_rate)
      write(12,*)
      write(12,*) 'END OF PROGRAM. '
      write(30,format_endmsg) msg, date, '//', timenow,']'
      endfile(30)
      close(30)
      close(12)
   	  stop
end program acru_menu_range
