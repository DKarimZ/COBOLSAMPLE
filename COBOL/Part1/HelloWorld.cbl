       identification division.
       program-id. HelloWorld.
       Author. DKisame.
      * Le traditionnel Hello World
       
       environment division.
       
       data division.
       working-storage section.
       01 NAME PIC A(20).
       
       procedure division.
       0100-START-HELLO-WORLD.
           DISPLAY "Hello World!!!".
           STOP RUN.
       
       end program Helloworld.