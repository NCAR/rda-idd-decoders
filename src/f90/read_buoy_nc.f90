PROGRAM read_buoy_nc

  USE netcdf
  IMPLICIT NONE
  
  CHARACTER (LEN = 128) :: nc_file
  
  INTEGER :: ncid

  INTEGER, PARAMETER :: hdr_len = 200
  INTEGER, PARAMETER :: outstg_len = 200

  CHARACTER (len = 1), ALLOCATABLE, DIMENSION(:,:) :: &
     ship     ! Ship call name

  INTEGER, ALLOCATABLE, DIMENSION(:) :: &
     buoy, &       ! Buoy number
     time_obs, &   ! Observation time (seconds since 1970-01-01 00 UTC)
     dir, &        ! Buoy/ship direction
     pwa, &        ! Wave period (seconds)
     hwa           ! Wave height (m)
     
  REAL, ALLOCATABLE, DIMENSION(:) :: &
     lat, &        ! Latitude
     lon, &        ! Longitude
     T, &          ! Air temperature (deg C)
     Td, &         ! Dew point temperature (deg C)
     slp, &        ! Sea level pressure (hPa)
     spd, &        ! Buoy/ship speed (m/s)
     Tw            ! Water temperature (deg C)

  INTEGER :: start(nf90_max_var_dims)
  INTEGER :: count(nf90_max_var_dims)
  INTEGER :: vdims(nf90_max_var_dims)
  INTEGER, DIMENSION(nf90_max_var_dims) :: vardimids
  
  INTEGER :: ndims, nvars, ngatts, unlimdimid
  INTEGER :: recNum
  INTEGER :: ship_call_len_id
  INTEGER :: ivarid, nrecs, ship_call_len

  CHARACTER (len = nf90_max_name) :: &
     recNumName, &
     ship_call_len_name, &
     varname, &
     dimname

  INTEGER :: vartype, nvardims, nvarAtts  
  INTEGER :: dimlen
  INTEGER :: i, j, lenstr, y, id
  
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
  call check( nf90_inq_dimid(ncid, 'ship_call_len', ship_call_len_id))
  call check( nf90_inquire_dimension(ncid, ship_call_len_id, ship_call_len_name, ship_call_len))
  
  write(6, *) ''
  write(6, fmt = '("NUMBER OF REPORTS: ", i)') recNum
  write(6, *) ''

! =======================================================================================
! Allocate memory

  ALLOCATE(ship(ship_call_len, recNum))

  ALLOCATE(buoy(recNum))
  ALLOCATE(time_obs(recNum))
  ALLOCATE(dir(recNum))
  ALLOCATE(pwa(recNum))
  ALLOCATE(hwa(recNum))

  ALLOCATE(lat(recNum))
  ALLOCATE(lon(recNum))
  ALLOCATE(T(recNum))
  ALLOCATE(Td(recNum))
  ALLOCATE(slp(recNum))
  ALLOCATE(spd(recNum))
  ALLOCATE(Tw(recNum))

! =======================================================================================
! Read in variables

  call get_character_var(ncid, 'ship', ship_call_len, recNum, ship)

  call get_integer_var(ncid, 'buoy', recNum, buoy)
  call get_integer_var(ncid, 'time_obs', recNum, time_obs)
  call get_integer_var(ncid, 'DIR', recNum, dir)
  call get_integer_var(ncid, 'Pwa', recNum, pwa)
  call get_integer_var(ncid, 'Hwa', recNum, hwa)

  call get_real_var(ncid, 'Lat', recNum, lat)
  call get_real_var(ncid, 'Lon', recNum, lon)
  call get_real_var(ncid, 'T', recNum, T)
  call get_real_var(ncid, 'TD', recNum, Td)
  call get_real_var(ncid, 'SLP', recNum, SLP)
  call get_real_var(ncid, 'SPD', recNum, SPD)
  call get_real_var(ncid, 'Tw', recNum, Tw)

! =======================================================================================
! Close NetCDF dataset

  call check( nf90_close(ncid))

! =======================================================================================
! Write output

  open(unit=21, file='buoy_text.out')

  fmt_header1 = '("# ",90("-"))'

  fmt_header2 = '(1x,a5,2x,a11,2x,a6,2x,a7,2x,a5,2x,a5, &
                  2x,a6,2x,a3,2x,a4,2x,a5,2x,a3,2x,a3,2x,a4)'

  fmt_record  = '(i6,2x,i11,2x,1f6.2,2X,1f7.2,2X,1f5.1,2X,1f5.1,2X,1f6.1, &
                  2x,i3,2x,1f4.1,2x,f5.1,2x,i3,2x,i3,2x,5a1)'

  write(unit=21, fmt = fmt_header1)
  write(unit=21, fmt = fmt_header2) 'BUOY','TIMEOBS','LAT','LON','TEMP','DPTEMP', &
                                    'SLP','DIR','WSPD','WaterT','PER','HGT','SHIP'
  write(unit=21, fmt = fmt_header1)

  do id = 1, recNum

      ! check for invalid characters in ship string
      do j = 1, ship_call_len
        if ((iachar(ship(j,id)) .le. 32) .or. (iachar(ship(j,id)) .ge. 127)) then
          ship(j,id) = " "
        endif
      enddo

      write (UNIT = outstg, fmt = fmt_record) &
         buoy(id), &
         time_obs(id), &
         lat(id), &
         lon(id), &
         T(id), &
         Td(id), &
         slp(id), &
         dir(id), &
         spd(id), &
         Tw(id), &
         pwa(id), &
         hwa(id), &
         (ship(j,id),j=1,5)
         
      do y = 1, outstg_len
        if ( outstg (y:y) .eq. '*') then
             outstg (y:y) = '9'
        endif
      enddo

      WRITE (UNIT = 21, FMT = '(A)' ) trim(outstg)

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

END PROGRAM read_buoy_nc