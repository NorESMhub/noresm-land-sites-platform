module table_manager

   use ptaero_table
   use commondefinitions
   use aerosoldef !for the mixture ids

   integer, parameter, public  :: table_property_dry_radius=1
   integer, parameter, public  :: table_property_sigma=2
   integer, parameter   :: max_number_of_properties=10  !Max # properties for a mixture

   TYPE(ptaero_table_t), target, dimension(max_number_of_properties*(nmodes+1)) :: property_table

   integer, dimension(0:nmodes,max_number_of_properties) :: property_table_index
   integer, save                                         :: last_used_index=0

contains

   subroutine initialize_tables()
      implicit none
      integer               :: i

      last_used_index = 0
      property_table_index(:,:) = -1 !Negative index means un-used

      do i=1,SIZE(property_table)
         !Construct an empty property table
         call construct(property_table(i)) 
      end do

   end subroutine initialize_tables

   !Registers a look-up table
   subroutine register_table(mixture_id, property_id, data2D, data3D, data4D, axis1, axis2, axis3, axis4)
      implicit none
      integer, intent(in)                                :: mixture_id
      integer, intent(in)                                :: property_id
      real(r8), dimension(:,:), intent(in), optional     :: data2D
      real(r8), dimension(:,:,:), intent(in), optional   :: data3D
      real(r8), dimension(:,:,:,:), intent(in), optional :: data4D
      real(r8), intent(in), dimension(:),optional        :: axis1
      real(r8), intent(in), dimension(:),optional        :: axis2
      real(r8), intent(in), dimension(:),optional        :: axis3
      real(r8), intent(in), dimension(:),optional        :: axis4

      !Increase the number of tables we are keeping track of..
      last_used_index = last_used_index + 1

      !Remember the placement of this table
      property_table_index(mixture_id, property_id) = last_used_index
      
      !Need to check for what kind of data the table contains..
      if(present(data2D))then
         call initialize(property_table(last_used_index) & !This is the table we are initializing 
                        , mixture_id                     & !id of the mixture
                        , property_id                    & !id of the property 
                        , limits1=axis1                  & !axis limits (grid) of first axis
                        , limits2=axis2                  & !axis limits (grid) of second axis
                        , data2d=data2d                  & !the 2d-data of the table
                        )
      else if(present(data3D))then
         call initialize(property_table(last_used_index) & !This is the table we are initializing 
                        , mixture_id                     & !id of the mixture
                        , property_id                    & !id of the property 
                        , limits1=axis1                  & !axis limits (grid) of first axis
                        , limits2=axis2                  & !axis limits (grid) of second axis
                        , limits3=axis3                  & !axis limits (grid) of third axis
                        , data3d=data3d                  & !the 2d-data of the table
                        )
      else if(present(data4D))then
         call initialize(property_table(last_used_index) & !This is the table we are initializing 
                        , mixture_id                     & !id of the mixture
                        , property_id                    & !id of the property 
                        , limits1=axis1                  & !axis limits (grid) of first axis
                        , limits2=axis2                  & !axis limits (grid) of second axis
                        , limits3=axis3                  & !axis limits (grid) of third axis
                        , limits4=axis3                  & !axis limits (grid) of third axis
                        , data4d=data4d                  & !the 2d-data of the table
                        )
      end if

   end subroutine register_table

   function get_table_pointer(mixture_id, property_id)RESULT(table_pointer)
      integer,intent(in)                           :: mixture_id
      integer,intent(in)                           :: property_id
      TYPE(ptaero_table_t), pointer                :: table_pointer

      nullify(table_pointer)

      table_pointer=>property_table(property_table_index(mixture_id, property_id))

   end function


end module table_manager
