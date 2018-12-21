PROGRAM read_metar_nc

  USE netcdf
  IMPLICIT NONE
  
  CHARACTER (LEN = 128) :: nc_file
  
  INTEGER :: ncid

  INTEGER, PARAMETER :: hdr_len = 200
  INTEGER, PARAMETER :: report_len = 192
  INTEGER, PARAMETER :: outstg_len = 200

  CHARACTER (len = 1), ALLOCATABLE, DIMENSION(:, :) :: &
     station_id, &
     report

  INTEGER, ALLOCATABLE, DIMENSION(:) :: &
     wmo_id, &
     altitude, &
     time_observation, &
     wind_from_direction, &
     parent_index

  REAL, ALLOCATABLE, DIMENSION(:) :: &
     latitude, &
     longitude, &
     wind_speed, &
     air_temperature, &
     dew_point_temperature, &
     inches_ALTIM, &
     hectoPascal_ALTIM, &
     air_pressure_at_sea_level
     
  INTEGER :: start(nf90_max_var_dims)
  INTEGER :: count(nf90_max_var_dims)
  INTEGER :: vdims(nf90_max_var_dims)
  INTEGER, DIMENSION(nf90_max_var_dims) :: vardimids
  
  INTEGER :: ndims, nvars, ngatts, unlimdimid
  INTEGER :: recNum, nstations, id_len
  INTEGER :: stationdimid, id_lendimid
  CHARACTER (len = nf90_max_name) :: recNumName, stationName, id_lenName
  
  INTEGER :: ivarid
  CHARACTER (len = nf90_max_name) :: varname

  INTEGER :: vartype, nvardims, nvarAtts
  
  character (len = nf90_max_name) :: dimname
  INTEGER :: dimlen
  
  INTEGER :: i, j, lenstr, h, y, id
  
  CHARACTER (len = hdr_len) :: fmt_header1, fmt_header2, fmt_record
  CHARACTER (len = outstg_len) :: outstg
  CHARACTER (len = report_len) :: report_outstg

! =======================================================================================
! Open NetCDF file and query dimensions, variables, and attributes

  write(6, *) "Enter input file name: "
  read(5, fmt = '(a128)') nc_file
  write(6, *) ''
  write(6, fmt = '("INPUT FILE: ",a)') trim(nc_file)
  
  call check( nf90_open(nc_file, nf90_nowrite, ncid) )
  call check( nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid) )

  write(6, *) ''
  write(6, fmt = '("NUMBER OF DIMENSIONS: ", i)') ndims
  write(6, fmt = '("NUMBER OF VARIABLES: ", i)') nvars
  write(6, fmt = '("NUMBER OF GLOBAL ATTRIBUTES: ", i)') ngatts
  
  call check( nf90_inquire_dimension(ncid, unlimdimid, recNumName, recNum))
  call check( nf90_inq_dimid(ncid, 'station', stationdimid))
  call check( nf90_inquire_dimension(ncid, stationdimid, stationName, nstations))
  call check( nf90_inq_dimid(ncid, 'id_len', id_lendimid))
  call check( nf90_inquire_dimension(ncid, id_lendimid, id_lenName, id_len))
  
  write(6, *) ''
  write(6, fmt = '("NUMBER OF REPORTS: ", i)') recNum
  write(6, fmt = '("NUMBER OF STATIONS: ", i)') nstations
  write(6, *) ''

! =======================================================================================
! Allocate memory

  ALLOCATE(station_id(id_len, recNum))
  ALLOCATE(report(report_len, recNum))
  
  ALLOCATE(wmo_id(nstations))
  ALLOCATE(altitude(nstations))
  ALLOCATE(latitude(nstations))
  ALLOCATE(longitude(nstations))

  ALLOCATE(parent_index(recNum))
  ALLOCATE(time_observation(recNum))

  ALLOCATE(wind_from_direction(recNum))
  ALLOCATE(wind_speed(recNum))
  ALLOCATE(air_temperature(recNum))
  ALLOCATE(dew_point_temperature(recNum))
  ALLOCATE(inches_ALTIM(recNum))
  ALLOCATE(hectoPascal_ALTIM(recNum))
  ALLOCATE(air_pressure_at_sea_level(recNum))
  
! =======================================================================================
! Get variables

  call get_character_var(ncid, 'station_id', id_len, nstations, station_id)
  call get_character_var(ncid, 'report', report_len, recNum, report)

  call get_integer_var(ncid, 'wmo_id', nstations, wmo_id)
  call get_integer_var(ncid, 'altitude', nstations, altitude)
  call get_integer_var(ncid, 'parent_index', recNum, parent_index)
  call get_integer_var(ncid, 'time_observation', recNum, time_observation)
  call get_integer_var(ncid, 'wind_from_direction', recNum, wind_from_direction)

  call get_real_var(ncid, 'latitude', nstations, latitude)
  call get_real_var(ncid, 'longitude', nstations, longitude)
  call get_real_var(ncid, 'wind_speed', recNum, wind_speed)
  call get_real_var(ncid, 'air_temperature', recNum, air_temperature)
  call get_real_var(ncid, 'dew_point_temperature', recNum, dew_point_temperature)
  call get_real_var(ncid, 'inches_ALTIM', recNum, inches_ALTIM)
  call get_real_var(ncid, 'hectoPascal_ALTIM', recNum, hectoPascal_ALTIM)
  call get_real_var(ncid, 'air_pressure_at_sea_level', recNum, air_pressure_at_sea_level)

! =======================================================================================
! Close NetCDF dataset

  call check( nf90_close(ncid))

! =======================================================================================
! Write output

  open(unit=21, file='metar_text.out')
  open(unit=22, file='metar_report.out')

  fmt_header1 = '("# ",145("-"))'
  fmt_header2 = '(1x,a3,2x,a5,2x,a5,2x,a10,2x,a5,2x,a7,2x,a5,&
                   2x,a5,2x,a5,2x,a3,2x,a8,2x,a8,2x,a6)'
  fmt_record  = '(4a1,2x,i6,2x,i5,2x,i10,2x,f6.2,2x,f7.2,2x,f5.1, &
                  2x,f5.1,2x,f7.2,2x,i3,2x,f8.2,2x,f8.2,2x,f6.2)'

  write(unit=21, fmt = fmt_header1)
  write(unit=21, fmt = fmt_header2) 'STA','WMOID','ELEV','ObsTime','LAT','LON', &
                                    'T','TD','SPD','DIR','SLP','MSLP','ALTIM'
  write(unit=21, fmt = fmt_header1)

  do id = 1, recNum
    h = parent_index(id) + 1

    if (h.gt.0) then

      ! check for invalid characters in station_id string
      do j = 1, id_len
        if ((iachar(station_id(j,h)) .le. 32) .or. (iachar(station_id(j,h)) .ge. 127)) then
          station_id(j,h) = " "
        endif
      enddo

      ! check for invalid characters in report string
      do j = 1, report_len
        if ((iachar(report(j,id)) .le. 32) .or. (iachar(report(j,id)) .ge. 127)) then
          report(j,id) = " "
        endif
      enddo

      write (unit = report_outstg, FMT = '(192a1)') (report(j,id), j=1,report_len)
      
        write (UNIT = outstg, fmt = fmt_record) &
               (station_id(j,h),j=1,4), &
               wmo_id(h), &
               altitude(h), &
               time_observation(id), &
               latitude(h), &
               longitude(h), &
               air_temperature(id), &
               dew_point_temperature(id), &
               wind_speed(id), &
               wind_from_direction(id), &
               hectoPascal_ALTIM(id), &
               air_pressure_at_sea_level(id), &
               inches_ALTIM(id) 
         
        do y = 1, outstg_len
          if ( outstg (y:y) .eq. '*') then
               outstg (y:y) = '9'
          endif
        enddo

        WRITE (UNIT = 21, FMT = '(A)' ) trim(outstg)
        WRITE (unit = 22, FMT = '(A)') report_outstg      
    endif

  enddo
  
  close(unit=21)
  close(unit=22)

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

END PROGRAM read_metar_nc