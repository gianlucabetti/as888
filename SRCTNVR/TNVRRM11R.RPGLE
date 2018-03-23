000100940224     H*--------------------------------------------------------------*
000200060928     Fdppdc02l  iF   E           k DISK
000300060928     Fazorg01l  iF   E           k DISK
000400060928     fqsysprt   o    F  132        printer
000500060928
000600060928     D fdp             S              3  0 DIM(20)
000700060928     d og143         E DS
000800060920      * - Ds per Anagrafiche Clienti
000900060920     d TIBS69DS      E DS
001000060920     d DS_cnaco      E DS                  extname(CNACO00F)
001100060920     d DS_cnind      E DS                  extname(CNIND00F)
001200060920     d DS_cnclp      E DS                  extname(CNCLP00F)
001300060920     d DS_fncls      E DS                  extname(FNCLS00F)
001400060928     d keyksc          s                   Like(pdcksc) inz(0)
001500900131     C*---------------------------------------------------------------*
001600060928     c                   z-add     0             ii                2 0
001700060928     c                   clear                   fdp
001800060928     c* caricamento da azorg di tutti i p.o. dpd
001900060928     c     *loval        setll     azorg01l
002000060928    1c                   do        *hival
002100060928     c                   read      azorg01l
002200060928    2c                   if        %eof(azorg01l)
002300060928     c                   leave
002400060928    2c                   endif
002500060928     c                   movel     orgde3        og143
002600060928    2c                   if        §ogntw='DPD'
002700060928     c                   add       1             ii
002800060928     c                   z-add     orgfil        Fdp(ii)
002900060928    2c                   endif
003000060928    1c                   enddo
003100060928     c* lettura di dppdc elabrando tutti i record senza cod.cliente pdc
003200060928     c     keyksc        setll     dppdc02l
003300060920    1c                   do        *hival
003400060928     c     keyksc        reade     dppdc02l
003500060928    2c                   if        %eof(dppdc02l)
003600060920     c                   leave
003700060920    2c                   endif
003800060928     c                   z-add     1             ii
003900060928    2c     ii            dowle     20
004000060928     c     fdp(ii)       andgt     0
004100060920     c                   clear                   tibs69ds
004200060928     c                   movel     fdp(ii)       i69kac
004300060920     c                   move      pdcdpc        i69kac
004400060920     C                   call      'TIBS69R'
004500060920     C                   parm                    tibs69DS
004600060920     C                   parm                    DS_cnaco
004700060920     C                   parm                    DS_cnind
004800060920     C                   parm                    DS_cnclp
004900060920     C                   parm                    DS_fncls
005000060928     c* se codice trovato stampo ed esco dal ciclo
005100060920    3c                   If         O69ERR =  ' '
005200060928     c                   except    P_rig
005300060928     c                   z-add     21            ii
005400060920     c                   else
005500060928     c* se cod. non trovato continuo il loop
005600060928     c                   add       1             ii
005700060920    3c                   endif
005800060928    2c                   enddo
005900060920    1c                   enddo
006000030205      *
006100000000     C                   SETON                                        LR
006200060928     Oqsysprt   e            P_rig
006300060928     O                       i69kac
006400060928     o                       pdcnar           +   2
