PROGRAM read_upperair_nc

  USE netcdf
  IMPLICIT NONE
  
  CHARACTER (LEN = 128) :: nc_file
  
  INTEGER :: ncid

  INTEGER, PARAMETER :: hdr_len = 200
  INTEGER, PARAMETER :: outstg_len = 200

  CHARACTER (len = 1), ALLOCATABLE, DIMENSION(:,:) :: &
     staName     ! Ship call name

  INTEGER, ALLOCATABLE, DIMENSION(:) :: &
     wmoStaNum     ! WMO station number

  REAL, ALLOCATABLE, DIMENSION(:) :: &
     lat, &        ! Latitude
     lon, &        ! Longitude
     elev          ! Station elevation (m)

  REAL, ALLOCATABLE, DIMENSION(:,:) :: &
     prMan, &      ! Pressure - mandatory level (hPa)
     htMan, &      ! Geopotential height - mandatory level (m)
     tpMan, &      ! Temperature - mandatory level (K)
     tdMan, &      ! Dew point depression - mandatory level (K)
     wdMan, &      ! Wind direction - mandatory level (degree_true)
     wsMan         ! Wind speed - mandatory level (m/s)

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: &
     synTime       ! Synoptic time (seconds since 1970-01-01 00 UTC)
     
  INTEGER :: start(nf90_max_var_dims)
  INTEGER :: count(nf90_max_var_dims)
  INTEGER :: vdims(nf90_max_var_dims)
  INTEGER, DIMENSION(nf90_max_var_dims) :: vardimids
  
  INTEGER :: ndims, nvars, ngatts, unlimdimid
  INTEGER :: recNum
  INTEGER :: staNameLenid, manLevelid, sigWLevelid, sigTLevelid
  INTEGER :: ivarid, nrecs
  INTEGER :: staNameLen, manLevel, sigTLevel, sigWLevel

  CHARACTER (len = nf90_max_name) :: &
     recNumName, &
     staNameLen_name, &
     manLevel_name, &
     sigTLevel_name, &
     sigWLevel_name, &
     varname, &
     dimname

  INTEGER :: vartype, nvardims, nvarAtts  
  INTEGER :: dimlen
  INTEGER :: i, j, lenstr, y, id, lev
  
  CHARACTER (len = hdr_len) :: fmt_header1, fmt_header2, fmt_record
  CHARACTER (len = outstg_len) :: outstg

! =======================================================================================
! Open NetCDF file and query dimensions, variables, and attributes

  write(6, *) "Enter input file name: "
  read(5, fmt = '(a128)') nc_file
  write(6, fmt = '("INPUT FILE: ",a)') trim(nc_file)
  
  call check( nf90_open(nc_file, nf90_nowrite, ncid) )
  call check( nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid) )

  write(6, *) ''
  write(6, fmt = '("NUMBER OF DIMENSIONS: ", i)') ndims
  write(6, fmt = '("NUMBER OF VARIABLES: ", i)') nvars
  write(6, fmt = '("NUMBER OF GLOBAL ATTRIBUTES: ", i)') ngatts

  call check( nf90_inquire_dimension(ncid, unlimdimid, recNumName, recNum))
  call check( nf90_inq_dimid(ncid, 'staNameLen', staNameLenid))
  call check( nf90_inquire_dimension(ncid, staNameLenid, staNameLen_name, staNameLen))
  call check( nf90_inq_dimid(ncid, 'manLevel', manLevelid))
  call check( nf90_inquire_dimension(ncid, manLevelid, manLevel_name, manLevel))
  call check( nf90_inq_dimid(ncid, 'sigTLevel', sigTLevelid))
  call check( nf90_inquire_dimension(ncid, sigTLevelid, sigTLevel_name, sigTLevel))
  call check( nf90_inq_dimid(ncid, 'sigWLevel', sigWLevelid))
  call check( nf90_inquire_dimension(ncid, sigWLevelid, sigWLevel_name, sigWLevel))
  
  write(6, *) ''
  write(6, fmt = '("NUMBER OF REPORTS: ", i)') recNum
  write(6, fmt = '("NUMBER OF MANDATORY LEVELS: ", i)') manLevel
  write(6, fmt = '("NUMBER OF SIGNIFICANT LEVELS WRT T: ", i)') sigTLevel
  write(6, fmt = '("NUMBER OF SIGNIFICANT LEVELS WRT W: ", i)') sigWLevel
  write(6, *) ''

! =======================================================================================
! Allocate memory

  ALLOCATE(staName(staNameLen, recNum))
  ALLOCATE(wmoStaNum(recNum))
  ALLOCATE(synTime(recNum))
  ALLOCATE(lat(recNum))
  ALLOCATE(lon(recNum))
  ALLOCATE(elev(recNum))
  ALLOCATE(prMan(manLevel, recNum))
  ALLOCATE(htMan(manLevel, recNum))
  ALLOCATE(tpMan(manLevel, recNum))
  ALLOCATE(tdMan(manLevel, recNum))
  ALLOCATE(wdMan(manLevel, recNum))
  ALLOCATE(wsMan(manLevel, recNum))

! =======================================================================================
! Read in variables

  call get_character_var(ncid, 'staName', staNameLen, recNum, staName)
  call get_integer_var(ncid, 'wmoStaNum', recNum, wmoStaNum)
  call get_double_var(ncid, 'synTime', recNum, synTime)
  call get_real_var(ncid, 'staLat', recNum, lat)
  call get_real_var(ncid, 'staLon', recNum, lon)
  call get_real_var(ncid, 'staElev', recNum, elev)
  call get_real_var(ncid, 'prMan', recNum, prMan)
  call get_real_var(ncid, 'htMan', recNum, htMan)
  call get_real_var(ncid, 'tpMan', recNum, tpMan)
  call get_real_var(ncid, 'tdMan', recNum, tdMan)
  call get_real_var(ncid, 'wdMan', recNum, wdMan)
  call get_real_var(ncid, 'wsMan', recNum, wsMan)

! =======================================================================================
! Close NetCDF dataset

  call check( nf90_close(ncid))

! =======================================================================================
! Write output

  open(unit=21, file='upperair_text.out')

  fmt_header1 = '("# ",102("-"))'

  fmt_header2 = '(1x,a5,2x,a7,2x,a7,2x,a12,2x,a6,2x,a6,2x,a8,&
                  2x,a5,2x,a4,2x,a5,2x,a5,2x,a7)'

  fmt_record  = '(1i6,2(2X,f7.2),2x,1f12.1,2X,1f6.1,2x,f6.1,2x,f8.1,&
                  2x,f5.1,2x,f4.1,2x,f5.1,2x,f5.1,2X,6a1)'

  write(unit=21, fmt = fmt_header1)
  write(unit=21, fmt = fmt_header2) 'WMOID','LAT','LON','SYNTIME','ELEV','PRES', &
                                    'HEIGHT','TEMP','DPTEMP','DIR','SPD','STANAME'
  write(unit=21, fmt = fmt_header1)

  do id = 1, recNum

      ! check for invalid characters in staName string
      do j = 1, staNameLen
        if ((iachar(staName(j,id)) .le. 32) .or. (iachar(staName(j,id)) .ge. 127)) then
          staName(j,id) = " "
        endif
      enddo

     do lev = 1, manLevel
        write (UNIT = outstg, fmt = fmt_record) &
           wmoStaNum(id), &
           lat(id), &
           lon(id), &
           synTime(id), &
           elev(id), &
           prMan(lev,id), &
           htMan(lev,id), &
           tpMan(lev,id), &
           tdMan(lev,id), &
           wdMan(lev,id), &
           wsMan(lev,id), &
           (staName(j,id),j=1,staNameLen)

        do y = 1, outstg_len
          if ( outstg (y:y) .eq. '*') then
               outstg (y:y) = '9'
          endif
        enddo

        WRITE (UNIT = 21, FMT = '(A)' ) trim(outstg)
     enddo 
  enddo
  
  close(unit=21)

CONTAINS

! =======================================================================================
  SUBROUTINE get_character_var(ncid, varname, dim_len, dimsize, values)
! =======================================================================================

  INTEGER, INTENT(IN) :: ncid, dim_len, dimsize
  CHARACTER (LEN = *), INTENT(IN) :: varname
  CHARACTER (LEN = 1), DIMENSION(dim_len, dimsize), INTENT(OUT) :: values
  CHARACTER (LEN = nf90_max_name) :: vname

  call check( nf90_inq_varid(ncid, varname, ivarid) )
  call check( nf90_inquire_variable(ncid, ivarid, &
                                    name = vname, &
                                    xtype = vartype, &
                                    ndims = nvardims, &
                                    dimids = vardimids, &
                                    natts = nvarAtts))

  lenstr = 1
      
  do j = 1, nvardims
    call check(nf90_inquire_dimension(ncid, vardimids(j), name = dimname, len = dimlen))
    lenstr = lenstr * dimlen
    start(j) = 1
    count(j) = dimlen
  enddo
            
  call check( nf90_get_var(ncid, ivarid, values, start = start, count = count) )
  write(*, fmt = '(a, " filled")') varname

! =======================================================================================
  END SUBROUTINE get_character_var
! =======================================================================================

! =======================================================================================
  SUBROUTINE get_integer_var(ncid, varname, dimsize, values)
! =======================================================================================

  INTEGER, INTENT(IN) :: ncid, dimsize
  CHARACTER (LEN = *), INTENT(IN) :: varname
  INTEGER, DIMENSION(dimsize), INTENT(OUT) :: values
  CHARACTER (LEN = nf90_max_name) :: vname

  call check( nf90_inq_varid(ncid, varname, ivarid) )
  call check( nf90_inquire_variable(ncid, ivarid, &
                                    name = vname, &
                                    xtype = vartype, &
                                    ndims = nvardims, &
                                    dimids = vardimids, &
                                    natts = nvarAtts))

  lenstr = 1
      
  do j = 1, nvardims
    call check(nf90_inquire_dimension(ncid, vardimids(j), name = dimname, len = dimlen))
    lenstr = lenstr * dimlen
    start(j) = 1
    count(j) = dimlen
  enddo
            
  call check( nf90_get_var(ncid, ivarid, values, start = start, count = count) )
  write(*, fmt = '(a, " filled")') varname

! =======================================================================================
  END SUBROUTINE get_integer_var
! =======================================================================================

! =======================================================================================
  SUBROUTINE get_real_var(ncid, varname, dimsize, values)
! =======================================================================================

  INTEGER, INTENT(IN) :: ncid, dimsize
  CHARACTER (LEN = *), INTENT(IN) :: varname
  REAL, DIMENSION(dimsize), INTENT(OUT) :: values
  CHARACTER (LEN = nf90_max_name) :: vname

  call check( nf90_inq_varid(ncid, varname, ivarid) )
  call check( nf90_inquire_variable(ncid, ivarid, &
                                    name = vname, &
                                    xtype = vartype, &
                                    ndims = nvardims, &
                                    dimids = vardimids, &
                                    natts = nvarAtts))

  lenstr = 1
      
  do j = 1, nvardims
    call check(nf90_inquire_dimension(ncid, vardimids(j), name = dimname, len = dimlen))
    lenstr = lenstr * dimlen
    start(j) = 1
    count(j) = dimlen
  enddo
            
  call check( nf90_get_var(ncid, ivarid, values, start = start, count = count) )
  write(*, fmt = '(a, " filled")') varname

! =======================================================================================
  END SUBROUTINE get_real_var
! =======================================================================================

! =======================================================================================
  SUBROUTINE get_double_var(ncid, varname, dimsize, values)
! =======================================================================================

  INTEGER, INTENT(IN) :: ncid, dimsize
  CHARACTER (LEN = *), INTENT(IN) :: varname
  DOUBLE PRECISION, DIMENSION(dimsize), INTENT(OUT) :: values
  CHARACTER (LEN = nf90_max_name) :: vname

  call check( nf90_inq_varid(ncid, varname, ivarid) )
  call check( nf90_inquire_variable(ncid, ivarid, &
                                    name = vname, &
                                    xtype = vartype, &
                                    ndims = nvardims, &
                                    dimids = vardimids, &
                                    natts = nvarAtts))

  lenstr = 1
      
  do j = 1, nvardims
    call check(nf90_inquire_dimension(ncid, vardimids(j), name = dimname, len = dimlen))
    lenstr = lenstr * dimlen
    start(j) = 1
    count(j) = dimlen
  enddo
            
  call check( nf90_get_var(ncid, ivarid, values, start = start, count = count) )
  write(*, fmt = '(a, " filled")') varname

! =======================================================================================
  END SUBROUTINE get_double_var
! =======================================================================================

! =======================================================================================
  SUBROUTINE check(status)
! =======================================================================================

    INTEGER, INTENT(IN) :: status
    
    if (status /= NF90_NOERR) then 
      write(6, fmt = '("Error: ", a)') trim(nf90_strerror(status))
      stop "Stopped"
    end if

! =======================================================================================
  END SUBROUTINE check  
! =======================================================================================

END PROGRAM read_upperair_nc