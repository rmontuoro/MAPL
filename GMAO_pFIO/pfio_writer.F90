
program main
   use pFIO_ConstantsMod, only : pFIO_s_tag, pFIO_m_w_tag, pFIO_w_m_tag
   use mpi 
   implicit none
   integer :: Inter_Comm
   integer :: ierr
   integer :: n, rank
   integer :: server_rank

   integer :: MPI_STAT(MPI_STATUS_SIZE)
   integer :: n_workers, i, idle, no_job, idle_worker
   integer :: command 
   integer, allocatable :: busy(:)
   
   call MPI_Init(ierr)

   call MPI_Comm_get_parent(Inter_Comm, ierr);

   call MPI_Comm_rank(MPI_COMM_WORLD,rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD,n_workers, ierr)

   allocate(busy(n_workers-1), source =0)
   print*, "n_worker, rank", n_workers, rank

   if( rank == 0 ) then
     do while (.true.)
       call MPI_recv( command, 1, MPI_INTEGER, &
                MPI_ANY_SOURCE, pFIO_s_tag, Inter_Comm, &
                MPI_STAT, ierr)
       server_rank = MPI_STAT(MPI_SOURCE)

       print*, "get the server rank, command", server_rank, command

       if (command == 1) then 
 
         idle_worker = 0

         do i = 1, n_workers -1
            if (busy(i) == 0) then
              idle_worker = i
              exit
            endif
         enddo

         if(idle_worker == 0) then ! get the idle_woker

             call MPI_recv( idle, 1, MPI_INTEGER, &
                MPI_ANY_SOURCE, pFIO_w_m_tag , MPI_COMM_WORLD, &
                MPI_STAT, ierr)
             idle_worker = idle
          endif
          ! tell server the idel_worker
          print*, "call MPI_send(idle_worker, 1, MPI_INTEGER, server_rank, pFIO_s_tag, Inter_Comm, ierr)", server_rank        
          call MPI_send(idle_worker, 1, MPI_INTEGER, server_rank, pFIO_s_tag, Inter_Comm, ierr)
          print*, "send to server done"
          print*, "start to send to worker"

          ! tell the idle_worker which server has work
          call MPI_send(server_rank, 1, MPI_INTEGER, idle_worker, pFIO_m_w_tag, MPI_COMM_WORLD, ierr)
          print*, "send to worker done"

          busy(idle_worker) = 1

       else

          no_job = -1
          do i = 1, n_workers -1
            if( busy(i) == 0) then
               call MPI_send(no_job, 1, MPI_INTEGER, i, pFIO_m_w_tag, MPI_COMM_WORLD, ierr)
            else
               call MPI_recv( idle, 1, MPI_INTEGER, &
                 i, pFIO_w_m_tag, MPI_COMM_WORLD, &
                 MPI_STAT, ierr)
               if (idle /= i ) stop ("idle should be i")
               call MPI_send(no_job, 1, MPI_INTEGER, i, pFIO_m_w_tag, MPI_COMM_WORLD, ierr)
            endif  
          enddo  
          exit
       endif
     enddo

   else 

     do while (.true.)
        print*, "call MPI_recv from master"
        call MPI_recv( server_rank, 1, MPI_INTEGER, &
               0, pFIO_m_w_tag, MPI_COMM_WORLD, &
               MPI_STAT, ierr)
        print*, "master gives me ", server_rank
        if (server_rank == -1 ) exit
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! do somthing with server should match with server
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        block
           integer :: bsize
           integer, allocatable :: buffer(:)
           
           call MPI_recv( bsize, 1, MPI_INTEGER, &
               server_rank, pFIO_s_tag, Inter_comm, &
               MPI_STAT, ierr)
           allocate(buffer(bsize))
           call MPI_recv( buffer, bsize, MPI_INTEGER, &
               server_rank, pFIO_s_tag, Inter_comm, &
               MPI_STAT, ierr)
           print*, "I got ", bsize, "buffer from ", server_rank
           deallocate(buffer)
        end block
  

        print*, " I am telling master I am ready to have more work", rank
        call MPI_send(rank, 1, MPI_INTEGER, 0, pFIO_w_m_tag, MPI_COMM_WORLD , ierr)

      enddo
   endif
     
   call MPI_Finalize(ierr)
end program
