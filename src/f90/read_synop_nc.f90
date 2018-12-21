PROGRAM read_synop_nc

  USE netcdf
  IMPLICIT NONE
  
  CHARACTER (LEN = 128) :: nc_file
  
  INTEGER :: ncid

  INTEGER, PARAMETER :: hdr_len = 200
  INTEGER, PARAMETER :: outstg_len = 200

  CHARACTER (len = 1), ALLOCATABLE, DIMENSION(:,:) :: &
     stnName     ! Station name

  INTEGER, ALLOCATABLE, DIMENSION(:) :: &
     wmoId, &          ! WMO identification number
     elev, &           ! Station elevation
     time_obs, &       ! Observation time
     time_nominal, &   ! Nominal time
     DIR, &            ! Wind direction
     SPD               ! Wind speed

  REAL, ALLOCATABLE, DIMENSION(:) :: &
     lat, &            ! Latitude
     lon, &            ! Longitude
     T, &              ! Air temperature
     Td, &             ! Dew point temperature
     SLP               ! Sea level pressure
     
  INTEGER :: start(nf90_max_var_dims)
  INTEGER :: count(nf90_max_var_dims)
  INTEGER :: vdims(nf90_max_var_dims)
  INTEGER, DIMENSION(nf90_max_var_dims) :: vardimids
  
  INTEGER :: ndims, nvars, ngatts, unlimdimid
  INTEGER :: recNum
  INTEGER :: stnName_lenid
  INTEGER :: ivarid, nrecs, stnName_len

  CHARACTER (len = nf90_max_name) :: &
     recNumName, &
     stnName_lenName, &
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
  call check( nf90_inq_dimid(ncid, 'stnName_len', stnName_lenid))
  call check( nf90_inquire_dimension(ncid, stnName_lenid, stnName_lenName, stnName_len))
  
  write(6, *) ''
  write(6, fmt = '("NUMBER OF REPORTS: ", i)') recNum
  write(6, *) ''

! =======================================================================================
! Allocate memory

  ALLOCATE(stnName(stnName_len, recNum))

  ALLOCATE(wmoId(recNum))
  ALLOCATE(elev(recNum))
  ALLOCATE(time_obs(recNum))
  ALLOCATE(time_nominal(recNum))
  ALLOCATE(DIR(recNum))
  ALLOCATE(SPD(recNum))

  ALLOCATE(lat(recNum))
  ALLOCATE(lon(recNum))
  ALLOCATE(T(recNum))
  ALLOCATE(Td(recNum))
  ALLOCATE(SLP(recNum))

! =======================================================================================
! Read in variables

  call get_character_var(ncid, 'stnName', stnName_len, recNum, stnName)

  call get_integer_var(ncid, 'wmoId', recNum, wmoId)
  call get_integer_var(ncid, 'elev', recNum, elev)
  call get_integer_var(ncid, 'time_obs', recNum, time_obs)
  call get_integer_var(ncid, 'time_nominal', recNum, time_nominal)
  call get_integer_var(ncid, 'SPD', recNum, SPD)
  call get_integer_var(ncid, 'DIR', recNum, DIR)

  call get_real_var(ncid, 'Lat', recNum, lat)
  call get_real_var(ncid, 'Lon', recNum, lon)
  call get_real_var(ncid, 'T', recNum, T)
  call get_real_var(ncid, 'TD', recNum, Td)
  call get_real_var(ncid, 'SLP', recNum, SLP)

! =======================================================================================
! Close NetCDF dataset

  call check( nf90_close(ncid))

! =======================================================================================
! Write output

  open(unit=21, file='synop_text.out')

  fmt_header1 = '("# ",95("-"))'

  fmt_header2 = '(1x,a3,4x,a5,2x,a5,2x,a11,2x,a11,2x,a6,2x,a7,2x,a5, &
                  2x,a5,2x,a6,2x,a3,2x,a4)'

  fmt_record  = '(6a1,2x,1i5,2x,i5,2x,i11,2x,i11,2x,1f6.2,2x,1f7.2,2x,1f5.1, &
                  2x,1f5.1,2x,1f6.1,2x,i3,2x,1f4.1)'

  write(unit=21, fmt = fmt_header1)
  write(unit=21, fmt = fmt_header2) 'STN','WMOID','ELEV','TIMENOM','TIMEOBS', &
                                    'LAT','LON','TEMP','DPTMP','SLP','DIR','WSPD'
  write(unit=21, fmt = fmt_header1)

  do id = 1, recNum

      ! check for invalid characters in stnName string
      do j = 1, stnName_len
        if ((iachar(stnName(j,id)) .le. 32) .or. (iachar(stnName(j,id)) .ge. 127)) then
          stnName(j,id) = " "
        endif
      enddo

      write (UNIT = outstg, fmt = fmt_record) &
             (stnName(j,id), j=1, stnName_len), &
             wmoID(id), &
             elev(id), &
             time_nominal(id), &
             time_obs(id), &
             lat(id), &
             lon(id), &
             T(id), &
             TD(id), &
             SLP(id), &
             DIR(id), &
             SPD(id) 
         
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

END PROGRAM read_synop_nc