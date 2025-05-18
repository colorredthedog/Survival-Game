program SurvivalGame
    implicit none
    integer :: choice1, choice2, choice3, choice4, choice5, choice6, choice8, potchoice1, potchoice2, plantchoice1, plantchoice2, plantchoice3, plantchoice4, wood_dropoff, metal_dropoff, leaves_dropoff, wood_in_world, wood_bag, wood_mined, attack_damage, metal_in_world, metal_bag, metal_mined, leaves_in_world, leaves_bag, leaves_mined
    integer :: metal_storage_limit, leaves_storage_limit, planks_storage_limit, firewood_storage_limit, wood_storage_baglimit, metal_storage_baglimit, leaves_storage_baglimit, wood_storage_limit, burnable_planks_storage_limit, metal_plate_storage_limit, soilfertillizer_storage_limit
    integer :: wood_storage, metal_storage, leaves_storage, planks_storage, firewood_storage, burnable_planks_storage, metal_plate_storage, soilfertillizer_storage
    integer :: II_warehouse_owned, II_workshop_owned, III_warehouse_owned, III_factory_owned
    real, parameter :: initial_wood_in_world = 100, initial_metal_in_world = 50, initial_leaves_in_world = 200, initial_wood_storage_baglimit = 15, initial_metal_storage_baglimit = 10, initial_leaves_storage_baglimit = 25, initial_wood_storage_limit = 15, initial_metal_storage_limit = 10, initial_leaves_storage_limit = 25
    real, parameter :: initial_planks_storage_limit = 0, initial_firewood_storage_limit = 0, initial_burnable_planks_storage_limit = 0, initial_metal_plate_storage_limit = 0, inital_soilfertillizer_storage_limit = 0
    real, parameter :: building1_cost1 = 10, building1_cost2 = 10, building2_cost1 = 10, building2_cost2 = 10, building2_cost3 = 15, building3_cost1 = 1, building3_cost2 = 10, building3_cost3 = 15, building3_cost4 = 20, building4_cost1 = 10, building4_cost2 = 10, building4_cost3 = 15, building4_cost4 = 5
    real, parameter :: item1_cost1 = 3, item1_cost2 = 5
    character(len=1) :: I_plant_pot1 = 'f', I_plant_pot2 = 'f'
    character(len=4) ::  I_plant_pot1_plant = 'None', I_plant_pot2_plant = 'None'
    integer, parameter :: lett_owned = 0, lett_seeds_owned = 0, mato_owned = 0, carr_owned = 0, tato_owned = 0, oats_owned = 0, whet_owned = 0, rice_owned = 0, appl_owned = 0, oran_owned = 0, pine_owned = 0, cumb_owned = 0, corn_owned = 0, soyb_owned = 0, sunf_owned = 0, onio_owned = 0, suga_owned = 0, bean_owned = 0
    wood_in_world = initial_wood_in_world
    metal_in_world = initial_metal_in_world
    leaves_in_world = initial_leaves_in_world
    wood_storage_baglimit = initial_wood_storage_baglimit
    metal_storage_baglimit = initial_metal_storage_baglimit
    leaves_storage_baglimit = initial_leaves_storage_baglimit
    wood_storage_limit = initial_wood_storage_limit
    metal_storage_limit = initial_metal_storage_limit
    leaves_storage_limit = initial_leaves_storage_limit
    planks_storage_limit = initial_planks_storage_limit
    firewood_storage_limit = initial_firewood_storage_limit
    burnable_planks_storage_limit = initial_burnable_planks_storage_limit
    metal_plate_storage_limit = initial_metal_plate_storage_limit
    soilfertillizer_storage_limit = inital_soilfertillizer_storage_limit
    wood_mined = 1
    wood_bag = 0
    metal_mined = 1
    metal_bag = 0
    leaves_mined = 5
    leaves_bag = 0
    wood_storage = 0
    metal_storage = 0
    leaves_storage = 0
    planks_storage = 0
    firewood_storage = 0
    burnable_planks_storage = 0
    metal_plate_storage = 0
    soilfertillizer_storage = 0
    II_warehouse_owned = 0
    II_workshop_owned = 0
    III_warehouse_owned = 0
    III_factory_owned = 0
    
    do
    print *, 'Resources In World'
    print *, 'Total Wood:  ', wood_in_world
    print *, 'Total Metal: ', metal_in_world
    print *, 'Total Leaves:', leaves_in_world
    print *, ' '
    print *, '1. Resource Storage'
    print *, '2. Mine Resources'
    print *, '3. Resource Drop Off'
    print *, '13. Plants'
    print *, '15. Buildings Owned'
    print *, '16. Items'
    print *, '18. Crafting'
    print *, '19. Buildings'
    print *, '20. Exit'
    read(*,*) choice1
    
    select case(choice1)
    case(1)
        print *, ' '
        print *, 'Backpack storage'
        print *, 'Wood:  ', wood_bag
        print *, 'Metal: ', metal_bag
        print *, 'Leaves:', leaves_bag
        print *, ' '
        print *, 'Storage'
        print *, 'I Wood:             ', wood_storage
        print *, 'I Metal:            ', metal_storage
        print *, 'I Leaves:           ', leaves_storage
        print *, 'II Soilfertillizer: ', soilfertillizer_storage
        print *, 'II Planks:          ', planks_storage
        print *, 'II Firewood:        ', firewood_storage
        print *, 'III Burnable Planks:', burnable_planks_storage
        print *, 'III Metal Plate:    ', metal_plate_storage
        print *, ' '
    case(2)
        print *, ' '
        print *, 'Resources to mine'
        print *, ' '
        print *, '1. Wood'
        print *, '2. Metal'
        print *, '3. Leaves'
        print *, ' '
        read(*,*) choice2
        select case(choice2)
            case(1)
                if (wood_in_world >= wood_mined .AND. wood_bag <= wood_storage_baglimit) then
                wood_in_world = wood_in_world - wood_mined
                wood_bag = wood_bag + wood_mined
                print *, 'You Mined: ', wood_mined
                else
                print *, 'You Have No Room'
                end if
            case(2)
                if (metal_in_world >= metal_mined .AND. metal_bag <= metal_storage_baglimit) then
                metal_in_world = metal_in_world - metal_mined
                metal_bag = metal_bag + metal_mined
                print *, 'You Mined: ', metal_mined
                else
                print *, 'You Have No Room'
                end if
            case(3)
                if (leaves_in_world >= leaves_mined .AND. leaves_bag <= leaves_storage_baglimit) then
                leaves_in_world = leaves_in_world - leaves_mined
                leaves_bag = leaves_bag + leaves_mined
                print *, 'You Mined: ', leaves_mined
                else
                print *, 'You Have No Room'
                end if
                case default
                print *, 'Invalid choice. Please select a valid option.'
                end select
    case(3)
        print *, ' '
        print *, 'Resources to drop off'
        print *, ' '
        print *, '1. Wood'
        print *, '2. Metal'
        print *, '3. Leaves'
        print *, ' '
        read(*,*) choice3
        select case(choice3)
        case(1)
            print *, 'How Much Wood You Want To Drop Off'
            read(*,*) wood_dropoff
            if (wood_storage <= wood_storage_limit .AND. wood_bag >= wood_dropoff) then
                wood_storage = wood_storage + wood_dropoff
                wood_bag = wood_bag - wood_dropoff
                print *, 'You Have Dropped Off: ', wood_dropoff
                wood_dropoff = wood_dropoff - wood_dropoff
                else
                if (wood_storage >= wood_storage_limit .AND. wood_bag >= wood_dropoff) then
                print *, 'You Have No Room'
                else
                print *, 'You Don''t Have Any Wood'
                end if
                end if
        case(2)
            print *, 'How Much Metal You Want To Drop Off'
            read(*,*) metal_dropoff
            if (metal_storage <= metal_storage_limit .AND. metal_bag >= metal_dropoff) then
                metal_storage = metal_storage + metal_dropoff
                metal_bag = metal_bag - metal_dropoff
                print *, 'You Have Dropped Off: ', metal_dropoff
                metal_dropoff = metal_dropoff - metal_dropoff
            else
                if (metal_storage >= metal_storage_limit .AND. metal_bag >= metal_dropoff) then
                print *, 'You Have No Room'
                else
                print *, 'You Don''t Have Any Metal'
                end if
                end if
        case(3)
            print *, 'How Much Leaves You Want To Drop Off'
            read(*,*) leaves_dropoff
            if (leaves_storage <= leaves_storage_limit .AND. leaves_bag >= leaves_dropoff) then
                leaves_storage = leaves_storage + leaves_dropoff
                leaves_bag = leaves_bag - leaves_dropoff
                print *, 'You Have Dropped Off: ', leaves_dropoff
                leaves_dropoff = leaves_dropoff - leaves_dropoff
            else
                if (leaves_storage >= leaves_storage_limit .AND. leaves_bag >= leaves_dropoff) then
                print *, 'You Have No Room'
                else
                print *, 'You Don''t Have Any Leaves'
                end if
                end if
            case default
            print *, 'Invalid choice. Please select a valid option.'
        end select
        case(13)
        print *, ' '
        print *, '1: Plant1'
        print *, '2: Plant2'
        print *, ' '
        read(*,*) potchoice1
        select case(potchoice1)
        case(1)
            if (I_plant_pot1 == 't') then
                print *, 'Good'
            else
                print *, 'You Don''t Have A Plant Pot Here'
            end if
        case(2)
            if (I_plant_pot2 == 't') then
                print *, 'Good'
            else
                print *, 'You Don''t Have A Plant Pot Here'
            end if
        case default
        print *, 'Invalid choice. Please select a valid option.'
        end select
    case(15)
             print *, ' '
             print *, 'Building Owned'
             print *, ' '
             print *, 'II Warehouse:', II_warehouse_owned
             print *, 'II Workshop:', II_workshop_owned
            print *, 'III Warehouse:', III_warehouse_owned
            print *, 'III Factory:', III_factory_owned
             print *, ' '
    case(16)
        print *, ' '
        print *, 'Items'
        print *, ' '
        print *, '1. I Plant Pot'
        print *, ' '
        read(*,*) potchoice2
        select case(potchoice2)
        case(1)
            if (wood_storage >= item1_cost1 .AND. soilfertillizer_storage >= item1_cost2) then
                if (I_plant_pot1 == 'f') then
                print *, 'You Built A Plant Pot At 1'
                I_plant_pot1 = 't'
                wood_storage = wood_storage - item1_cost1
                soilfertillizer_storage = soilfertillizer_storage - item1_cost2
            else
                if (I_plant_pot2 == 'f') then
                    print *, 'You Built A Plant Pot At 2'
                    I_plant_pot2 = 't'
                    wood_storage = wood_storage - item1_cost1
                    soilfertillizer_storage = soilfertillizer_storage - item1_cost2
                else
                print *, 'You Alright Have All The Pot Available'
                end if
                end if
            else
            print *, 'You Don''t Have Enough Wood Or Soilfertillizer'
            end if
        case default
        print *, 'Invalid choice. Please select a valid option.'
        end select
    case(18)
        print *, ' '
        print *, 'Craft'
        print *, ' '
        print *, '1. Planks With Workshop'
        print *, '2. Firewood With Workshop'
        print *, '3. Soilfertillizer With Workshop'
        print *, ' '
        read(*,*) choice4
        select case(choice4)
        case(1)
        if (II_workshop_owned >= 1 .AND. planks_storage <= planks_storage_limit .AND. wood_storage >= 1) then
            wood_storage = wood_storage - 1
            planks_storage = planks_storage + 1
            print *, 'You Made A Plank'
        else
            print *, 'Make Sure To Have A II Workshop And II Warehouse'
        endif
        case(2)
        if (II_workshop_owned >= 1 .AND. firewood_storage <= firewood_storage_limit .AND. wood_storage >= 3) then
            wood_storage = wood_storage - 3
            firewood_storage = firewood_storage + 2
            print *, 'You Made A Firewood'
        else
            print *, 'Make Sure To Have A II Workshop And II Warehouse'
        endif
        case(3)
        if (II_workshop_owned >= 1 .AND. soilfertillizer_storage <= soilfertillizer_storage_limit .AND. leaves_storage >= 5) then
            leaves_storage = leaves_storage - 5
            soilfertillizer_storage = soilfertillizer_storage + 1
            print *, 'You Made A Soilfertillizer'
        else
            print *, 'Make Sure To Have A II Workshop And II Warehouse'
        endif
        case default
        print *, 'Invalid choice. Please select a valid option.'
        end select
        case(19)
        print *, ' '
        print *, 'Building to build'
        print *, ' '
        print *, '1. II Warehouse'
        print *, '2. II Workshop'
        print *, '3. III Warehouse'
        print *, '4. III Factory'
        print *, ' '
        read(*,*) choice5
        select case(choice5)
        case(1)
            if (building1_cost1 <= wood_storage .AND. building1_cost2 <= metal_storage) then
                wood_storage = wood_storage - building1_cost1
                metal_storage = metal_storage - building1_cost2
                II_warehouse_owned = II_warehouse_owned + 1
                wood_storage_limit = wood_storage_limit + 15
                metal_storage_limit = metal_storage_limit + 10
                leaves_storage_limit = leaves_storage_limit + 25
                planks_storage_limit = planks_storage_limit + 10
                firewood_storage_limit = firewood_storage_limit + 10
                print *, 'You Built A II Warehouse!'
            else
                print *, 'You Don''t Have Enough Resources To Build This'
            endif
        case(2)
            if (building2_cost1 <= wood_storage .AND. building2_cost2 <= metal_storage .AND. building2_cost3 <= leaves_storage) then
                wood_storage = wood_storage - building2_cost1
                metal_storage = metal_storage - building2_cost2
                leaves_storage = leaves_storage - building2_cost3
                II_workshop_owned = II_workshop_owned + 1
                print *, 'You Built A II Workshop!'
            else
                print *, 'You Don''t Have Enough Resources To Build This'
            endif
        case(3)
            if (building3_cost1 <= II_workshop_owned .AND. building3_cost2 <= planks_storage .AND. building3_cost3 <= metal_storage .AND. building3_cost4 <= leaves_storage) then
                II_workshop_owned = II_workshop_owned - building3_cost1
                planks_storage = planks_storage - building3_cost2
                metal_storage = metal_storage - building3_cost3
                leaves_storage = leaves_storage - building3_cost4
                III_warehouse_owned = III_warehouse_owned + 1
                wood_storage_limit = wood_storage_limit + 20
                metal_storage_limit = metal_storage_limit + 20
                leaves_storage_limit = leaves_storage_limit + 35
                planks_storage_limit = planks_storage_limit + 30
                firewood_storage_limit = firewood_storage_limit + 30
                print *, 'You Built A III Warehouse!'
            else
                print *, 'You Don''t Have Enough Resources To Build This'
            endif
        case(4)
            if (building4_cost1 <= planks_storage .AND. building4_cost2 <= metal_storage .AND. building4_cost3 <= leaves_storage .AND. building4_cost4 <= firewood_storage) then
                planks_storage = planks_storage - building4_cost1
                metal_storage = metal_storage - building4_cost2
                leaves_storage = leaves_storage - building4_cost3
                firewood_storage = firewood_storage - building4_cost4
                III_factory_owned = III_factory_owned + 1
                print *, 'You Built A III Factory!'
            else 
                print *, 'You Don''t Have Enough Resources To Build This'
            endif
        case default
        print *, 'Invalid choice. Please select a valid option.'
        end select
    case(20)
    exit
    case default
    print *, 'Invalid choice. Please select a valid option.'
    end select
    end do
    print *, 'Exiting the simulation. Thank you for playing!'
    end program SurvivalGame
