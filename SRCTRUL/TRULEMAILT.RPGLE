000100071122     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200061201
000300061201      //********************************************************************************************
000400061201      //
000500061201      // Definizione costanti.
000600061201      //
000700061201      //********************************************************************************************
000800061201     D Add...
000900061201     D                 C                   'A'
001000061201     D Replace...
001100061201     D                 C                   'R'
001200061201
001300061201      //********************************************************************************************
001400061201      //
001500061201      // Definizione procedure esterne.
001600061201      //
001700061201      //********************************************************************************************
001800061201     Demail_setAddressFromUser...
001900061201     D                 PR            10I 0
002000061201     D  rqsUtente                    10A   CONST
002100061201     D  rqsAddReplace                 1A   CONST
002200061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002300061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
002400061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
002500061201     D email_addAddress...
002600061201     D                 PR            10I 0
002700061201     D  rqsIndirizzo                   *   VALUE
002800061201     D                                     OPTIONS(*STRING:*TRIM)
002900061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
003000061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
003100061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003200061201     D email_send      PR            10I 0
003300061201     D  rqsOggetto                     *   VALUE OPTIONS(*STRING:*TRIM)
003400061201     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
003500061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
003600061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
003700061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003800061201     Demail_sndBrkMsg  PR            10I 0
003900061201     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
004000061201     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004100061201     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
004200061201     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004300061201
004400061201      //********************************************************************************************
004500061201      //
004600061201      // Definizione strutture dati.
004700061201      //
004800061201      //********************************************************************************************
004900061201     D psds           SDS
005000061201     D  jobName              244    253A
005100061201     D  jobUser              254    263A
005200061201     D  jobNumber            264    269A
005300061201
005400061201      /FREE
005500071122
005600061201       *INLR = *ON;
005700071122
005800061201       IF email_setAddressFromUser(jobUser : Replace) < 0;
005900061201         RETURN;
006000061201       ENDIF;
006100071122
006200071122       //IF email_setAddressFromUser('EDP*' : Add) < 0;
006300071122       //  RETURN;
006400071122       //ENDIF;
006500071122
006600061201       IF email_addAddress('danilo.cussini@tiscali.it') < 0;
006700061201         RETURN;
006800061201       ENDIF;
006900071122
007000071122       IF email_send('Inviata col service program TRULEMAIL.'
007100071122       : 'Questa email è stata inviata da un programma RPG -
007200071122       col service program TRULEMAIL e il comando SNDDST.'
007300071122       ) < 0;
007400071122         email_sndBrkMsg('Inviata col service program TRULEMAIL.');
007500061201       ENDIF;
007600071122
007700061201       RETURN;
007800071122
007900061201      /END-FREE
