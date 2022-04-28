module ptaero_table
   use shr_kind_mod,     only: r8 => shr_kind_r8
   implicit none
   save

   integer, parameter             :: max_table_rank = 4

   type one_dim_array_t
      real(r8), allocatable       :: values(:)
   end type

   type ptaero_table_t
      integer                            :: rank                  !table rank 
      integer, dimension(max_table_rank) :: dims                  !Dimension
      real(r8), dimension(:,:,:,:), allocatable :: values         !Table data
      TYPE(one_dim_array_t), dimension(:), allocatable  :: axisValues !axis values
   end type ptaero_table_t



contains

   subroutine construct(table)
      TYPE(ptaero_table_t), intent(inout)  :: table

      if(allocated(table%values))deallocate(table%values)
      if(allocated(table%values))deallocate(table%axisValues)
      table%dims(:)=1
      table%rank = 0
   end subroutine construct


   subroutine initialize(table, mixture_id, property_id, limits1, limits2, limits3, limits4, data2d, data3d, data4d)
      implicit none
      TYPE(ptaero_table_t), intent(inout)   :: table
      integer, intent(in)            :: mixture_id   !mixture id
      integer, intent(in)            :: property_id  !property (e.g. ssa, sigma, radius, whatever)
      real(r8), dimension(:,:), intent(in), optional :: data2d     !2d data 
      real(r8), dimension(:,:,:), intent(in), optional :: data3d   !3d data
      real(r8), dimension(:,:,:,:), intent(in), optional :: data4d   !4d data
      real(r8), dimension(:),intent(in),optional           :: limits1
      real(r8), dimension(:), intent(in),optional          :: limits2
      real(r8), dimension(:), intent(in),optional          :: limits3
      real(r8), dimension(:), intent(in),optional          :: limits4

      integer                                            :: i

      !Local variables
      logical tableFound

      !local
      tableFound=.FALSE.
      
      if(allocated(table%values))then
         stop "error"
      end if

      !Find the rank
      if(present(data2d))then
         table%rank = 2
         tableFound = .TRUE.
         table%dims(1) = SIZE(data2d,1)
         table%dims(2) = SIZE(data2d,2)
      endif
      if(present(data3d))then
         table%rank = 3
         if(tableFound .eqv. .TRUE.)then
            stop "error"
         end if
         tableFound=.TRUE.
         table%dims(1) = SIZE(data3d,1)
         table%dims(2) = SIZE(data3d,2)
         table%dims(3) = SIZE(data3d,3)
      end if
      if(present(data4d))then
         table%rank = 4
         if(tableFound .eqv. .TRUE.)then
            stop "error"
         end if
         tableFound=.TRUE.
         table%dims(1) = SIZE(data4d,1)
         table%dims(2) = SIZE(data4d,2)
         table%dims(3) = SIZE(data4d,3)
         table%dims(4) = SIZE(data4d,4)
      end if

      allocate(table%values(table%dims(1), table%dims(2), table%dims(3), table%dims(4)))

      !Allocate space for axis values
      allocate(table%axisValues(table%rank))
      do i=1,table%rank
         allocate(table%axisValues(i)%values(table%dims(i)))
      end do

      do i=1,table%rank
         select case(i)
            case(1)
            table%axisValues(i)%values(:)=limits1(:)
            case(2)
            table%axisValues(i)%values(:)=limits2(:)
            case(3)
            table%axisValues(i)%values(:)=limits3(:)
            case(4)
            table%axisValues(i)%values(:)=limits4(:)
         end select
       end do

       select case(table%rank)
       case(2)
         table%values(:,:,1,1) = data2d(:,:)
       case(3)
         table%values(:,:,:,1) = data3d(:,:,:)
       case(4)
         table%values(:,:,:,:) = data4d(:,:,:,:)
       end select

   end subroutine initialize

   !Search for the property along the array limits
   recursive function binary_search(arraylimits, numberToFind, iGuess, iLow, iHigh) result(lowLimitIndex)
      implicit none
      real(r8), dimension(:),intent(in)       :: arrayLimits  !Limits along the axis we are searching
      real(r8),              intent(in)       :: numberToFind !The property we are trying to find along the axis

      integer, intent(inout)                     :: iGuess       !Guessed index
      integer,intent(inout)                      :: iLow         !Lowest possible index
      integer, intent(inout)                     :: iHigh        !Highest possible index
      
      integer                                 :: lowLimitIndex

      if(numberToFind .lt. arrayLimits(iGuess))then
         iHigh = iGuess
         iGuess = int(0.5_r8*(iLow+iHigh))
         lowLimitIndex=binary_search(arrayLimits, numberToFind, iGuess, iLow, iHigh)
      else if (numberToFind .gt. arrayLimits(iGuess+1))then
         iLow = iGuess+1
         iGuess = int(0.5_r8*(iLow+iHigh))
         lowLimitIndex = binary_search(arrayLimits, numberToFind, iGuess, iLow, iHigh)
      else !property is between iGuess and iGuess+1 ==> we are ok
         lowLimitIndex = iLow
      end if
   end function binary_search


   !Search and obtain the value   
   function searchGetValue(table, axisValuesToFind ) RESULT(output)
      implicit none
      TYPE(ptaero_table_t), intent(in)   :: table
      real(r8), dimension(:), intent(in) :: axisValuesToFind          !array of numbers on axis

      !local variables
      integer, dimension(max_table_rank) :: lowLimit             !index of axis value below
      integer, dimension(max_table_rank) :: highLimit          !index of axis value above
      real(r8), dimension(max_table_rank):: lowFraction
      real(r8), dimension(max_table_rank):: highFraction
      real(r8)                           :: output

      integer                            :: iGuess
      integer                            :: iLow
      integer                            :: iHigh
      integer                            :: i

      !Get the indexes in question
      do i = 1, table%rank
         iLow=1
         iHigh = table%dims(i)
         iGuess = int(0.5_r8*(iLow+iHigh))  !Guess middle value
         lowLimit(i) = binary_search(table%axisValues(i)%values, axisValuesToFind(i), iGuess, iLow, iHigh )
         highLimit(i) = lowLimit(i)+1
      end do

      do i=1,table%rank
         !High fraction is distance to low value divided by total distance
         highFraction(i) = (axisValuesToFind(i)-table%axisValues(i)%values(lowLimit(i)))   &  
                           /(table%axisValues(i)%values(highLimit(i)) - table%axisValues(i)%values(lowLimit(i)))
         !Low fraction is one minus high
         lowFraction(i)  = 1.0_r8 - highFraction(i)
      end do

      !Interpolate along table limits
      call interpolate(table, lowLimit, highLimit, lowFraction, highFraction,output)

   end function searchGetValue

   
   !******************************************************************
   !Interpolate given that you know which indexes you are interested in
   subroutine interpolate(table, lowLimits, highLimits, lowFraction, highFraction,answer)
      implicit none
      type(ptaero_table_t),intent(in)     :: table
      integer, intent(in), dimension(:) :: lowLimits
      integer, intent(in), dimension(:) :: highLimits
      real(r8), intent(in), dimension(:) :: lowFraction
      real(r8), intent(in), dimension(:) :: highFraction
      real(r8), intent(out)              :: answer

      real(r8) , dimension(2,2,2) :: tmp3D
      real(r8) , dimension(2,2)   :: tmp2D
      real(r8) , dimension(2)     :: tmp1D     

      if(table%rank .eq. 3) then
         call extract3D(table%values(:,:,:,1), tmp3D, lowLimits, highLimits)
         call interpolate3D(tmp3D, tmp2D, tmp1d, answer, highFraction, lowFraction)
      elseif(table%rank .eq. 2) then
         !call extract2D(table%values(:,:,1,1), tmp2D, lowLimits, highLimits)
         call interpolate2D(tmp2D, tmp1d, answer, lowFraction, highFraction)
      else if (table%rank .eq. 1)then
         !call extract1D(table%values(:,1,1,1,1), tmp1D, lowLimits, highLimits)
         call interpolate1D(tmp1D, answer, lowFraction, highFraction)
      end if

   end subroutine 

   !Extract a compact 3D-array from the table
   subroutine extract3D(tmpIn, tmpOut, lowLimits, highLimits)
      implicit none
      real(r8), dimension(:,:,:),intent(in)  :: tmpIn                 !Full data array
      real(r8), dimension(2,2,2),intent(out) :: tmpOut                !Extracted, condensed array
      integer, dimension(:)              :: lowLimits
      integer, dimension(:)              :: highLImits

      tmpOut(1,1,1)=tmpIn(lowLimits(1),lowLimits(2),lowLimits(3))
      tmpOut(1,1,2)=tmpIn(lowLimits(1),lowLimits(2),highLimits(3))

      tmpOut(1,2,1)=tmpIn(lowLimits(1),highLimits(2),lowLimits(3))
      tmpOut(1,2,2)=tmpIn(lowLimits(1),highLimits(2),highLimits(3))

      tmpOut(2,1,1)=tmpIn(highLimits(1),lowLimits(2),lowLimits(3))
      tmpOut(2,1,2)=tmpIn(highLimits(1),lowLimits(2),highLimits(3))

      tmpOut(2,2,1)=tmpIn(highLimits(1),highLimits(2),lowLimits(3))
      tmpOut(2,2,2)=tmpIn(highLimits(1),highLimits(2),highLimits(3))
   end subroutine 

   !Remove dimension 3 and send back a 2d-array
   subroutine interpolate3D(tmp3D, tmp2D, tmp1d, answer, lowFraction, highFraction)
      implicit none
      real(r8), dimension(2,2,2), intent(in)   :: tmp3D
      real(r8), dimension(2,2),intent(inout)   :: tmp2D
      real(r8), dimension(2), intent(inout)    :: tmp1d
      real(r8)                                 :: answer
      real(r8), intent(in), dimension(:) :: highFraction
      real(r8), intent(in), dimension(:) :: lowFraction

      tmp2D(1,1) = lowFraction(3)*tmp3D(1,1,1) + highFraction(3)*tmp3D(1,1,2)
      tmp2D(1,2) = lowFraction(3)*tmp3D(1,2,1) + highFraction(3)*tmp3D(1,2,2)
      tmp2D(2,1) = lowFraction(3)*tmp3D(2,1,1) + highFraction(3)*tmp3D(2,1,2)
      tmp2D(2,2) = lowFraction(3)*tmp3D(2,2,1) + highFraction(3)*tmp3D(2,2,2)

      call interpolate2D(tmp2D, tmp1D, answer, lowFraction, highFraction)

   end subroutine

   !Remove dimension 2 and send back a 1d-array
   subroutine interpolate2D(tmp2D, tmp1D, answer, lowFraction, highFraction)
      implicit none
      real(r8), dimension(2,2),intent(in)    :: tmp2D
      real(r8), dimension(2),  intent(inout) :: tmp1D
      real(r8), dimension(:), intent(in)     :: lowFraction
      real(r8), dimension(:), intent(in)     :: highFraction
      real(r8),intent(out)                   :: answer

      tmp1D(1) = lowFraction(2)*tmp2D(1,1) + highFraction(2)*tmp2D(1,2)
      tmp1D(2) = lowFraction(2)*tmp2D(2,1) + highFraction(2)*tmp2D(2,2)

      call interpolate1D(tmp1D, answer, lowFraction, highFraction) 
   end subroutine interpolate2D   

   !Interpolate a 1d-array
   subroutine interpolate1D(tmp1D, answer, lowFraction, highFraction)
      implicit none
      real(r8), intent(in), dimension(2)   :: tmp1D
      real(r8), intent(out)                :: answer
      real(r8), intent(in), dimension(:)   :: lowFraction
      real(r8), intent(in), dimension(:)   :: highFraction

      answer = lowFraction(1)*tmp1D(1) + highFraction(1)*tmp1D(2)
   end subroutine interpolate1D

end module ptaero_table
