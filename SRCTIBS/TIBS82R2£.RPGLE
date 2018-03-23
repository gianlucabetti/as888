000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200050105      * TIBS82R2 *----------------------------------------------------*
000300050105      *  - MODIFICA PUNTO OPERATIVO: sistemo bolle transito    BTP
000400940915      *--------------------------------------------------------------*
000500050105     Ffnbtp15l  UF   E           K DISK
000600050105     FfnbtT11l  UF   E           K DISK
000700050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000800960302     D*
000900050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001000050105     d kflp            s                   Like(btpflp)
001100050105     d kdut            s                   Like(btpdut)
001200050105     d waggiorna       s              1
001300050105     d Dataiso         s               d   datfmt(*iso)
001400040917     C**********************************************************************
001500050105     c     kbtp          KLIST
001600050105     c                   KFLD                    kflp
001700050105     c                   KFLD                    kdut
001800050105     c     kbtt          KLIST
001900050105     c                   KFLD                    BtPflp
002000050105     c                   KFLD                    BtPAAS
002100050105     c                   KFLD                    BtPLNP
002200050105     c                   KFLD                    BtPNRS
002300050105     c                   KFLD                    BtPNSP
002400050103     C**********************************************************************
002500040917     C     *ENTRY        PLIST
002600050103     C                   PARM                    parpo             3 0
002700050103     C                   PARM                    partpo            3 0
002800050103     C                   PARM                    partpn            3 0
002900050105     c* Giro  udate
003000050105     c                   Move      *date         Dataiso
003100050105     c                   Move      dataiso       dateu             8 0
003200050103     c
003300050103     c                   eval      comman=ovrp(1)
003400050103     C                   CALL      'QCMDEXC'
003500050103     C                   PARM                    COMMAN           80
003600050103     C                   PARM      80            LUNG             15 5
003700050103     c
003800050103     c                   open      qsysprt
003900050103     c
004000050105     c* Leggo tutte le bolle transito con btpflp=new terminal e dut=0
004100050105     c                   eval      kflp=partpn
004200050105     c                   eval      kdut=0
004300050105     c
004400050105     c     kbtp          setll     fnbtp15l
004500050105     c     kbtp          reade     fnbtp15l
004600050105    1c                   dow       not %eof(fnbtp15l)
004700050103     c                   clear                   welabora          1
004800050103    2c                   if        parpo=partpo
004900050105    3c                   if        btptfp=parpo
005000050103     c                   eval      welabora='S'
005100050103    3c                   endif
005200050103     c
005300050103   X2c                   else
005400050103     c* altrimenti elaboro solo la linea
005500050105    3c                   if        btplnp=parpo
005600050103     c                   eval      welabora='S'
005700050103    3c                   endif
005800050103    2c                   endif
005900050103     c
006000050103    2c                   if        welabora='S'
006100050105     c* Se det =0 stampo e deleto
006200050105    3c                   if        btpdet=0
006300050105     c                   except    btperr1
006400050105     c
006500050105     c* D=deleta record
006600050105     c                   eval      waggiorna='D'
006700050105     c                   exsr      aggbtt
006800050105     c                   delete    fnbtp000
006900050105     c                   add       1             wtotdelet1        7 0
007000050105   x3c                   else
007100050105     c                   except    btperr1
007200050105     c* C=chiudi record
007300050105     c                   eval      waggiorna='C'
007400050105     c                   exsr      aggbtt
007500050105     c                   eval      btpdut=dateu
007600050105     c                   update    fnbtp000
007700050105     c                   add       1             wtotagg1          7 0
007800050105     c
007900050103    3c                   endif
008000050103    2c                   endif
008100050105     c
008200050105     c     kbtp          reade     fnbtp15l
008300050105    1C                   ENDDO
008400050105     c
008500050105     c* Leggo tutte le bolle transito con btpflp=old p.o.   e dut=0
008600050105     c                   eval      kflp=parpo
008700050105     c                   eval      kdut=0
008800050105     c
008900050105     c     kbtp          setll     fnbtp15l
009000050105     c     kbtp          reade     fnbtp15l
009100050105    1c                   dow       not %eof(fnbtp15l)
009200050105     c                   setoff                                       99
009300050105     c
009400050105     c* Se det =0 stampo e deleto
009500050105    2c                   if        btpdet=0
009600050105     c                   except    btperr2
009700050105     c
009800050105     c* D=deleta record
009900050105     c                   eval      waggiorna='D'
010000050105     c                   exsr      aggbtt
010100050105     c                   delete    fnbtp000
010200050105     c                   add       1             wtotdelet2        7 0
010300050105   x2c                   else
010400050105     c                   except    btperr2
010500050105     c* M=modifica p.o. transito nel nuovo terminal
010600050105     c                   eval      waggiorna='M'
010700050105     c                   exsr      aggbtt
010800050105     c                   eval      btpflp=partpn
010900050105     c                   update    fnbtp000                             99
011000050105    3c                   if        *in99
011100050105     c                   except    btperr2
011200050105     c                   add       1             wtoterr2          7 0
011300050105     c                   delete    fnbtp000
011400050105     c                   else
011500050105     c                   add       1             wtotagg2          7 0
011600050105    3c                   endif
011700050105     c
011800050105    2c                   endif
011900050105     c
012000050105     c     kbtp          reade     fnbtp15l
012100050105    1C                   ENDDO
012200050103     c
012300050103     c* Stampo totale bolle aggiornate
012400050103     c                   except    totale
012500050103     c
012600050103     c                   seton                                        lr
012700000828      *
012800951025     C**************************************************************************
012900951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
013000951025     C**************************************************************************
013100050105     C     aggbtt        BEGSR
013200020213
013300050105     C     kbtt          setll     fnbtt11l
013400050105     C     kbtt          reade     fnbtt11l
013500050105     c                   dow       not %eof(fnbtt11l)
013600050105     c                   select
013700050105     c                   when      waggiorna='D'
013800050105     c                   delete    fnbtt000
013900050105
014000050105     c                   when      waggiorna='C'
014100050105     c                   eval      bttdut=dateu
014200050105     c                   update    fnbtt000
014300050105
014400050105     c                   when      waggiorna='M'
014500050105     c                   eval      bttflp=partpn
014600050105     c                   update    fnbtt000                             99
014700050105     c                   if        *in99
014800050105     c                   delete    fnbtt000
014900050104     c                   endif
015000050105     c                   endsl
015100050105     c
015200050105     C     kbtt          reade     fnbtt11l
015300050103     c                   enddo
015400050103     c
015500951025     C                   ENDSR
015600050105     oQsysprt   e            BTPERR1        1
015700050105     o                                         +  0 'Bolla che transita dal '
015800050105     o                                         +  0 'nuovo terminal partenza:'
015900050105     o                       BtPflp            +  1
016000050105     o                                         +  2 'Bolla'
016100050105     o                       BtPAAS            +  2
016200050105     o                       BtPLNP            +  1
016300050105     o                       BtPNRS        z   +  1
016400050105     o                       BtPNSP        2   +  1
016500050105     o                                         +  2 'Entrata al transito:'
016600050105     o                       BtPdet            +  1 '    /  /  '
016700050105     oQsysprt   e            BTPERR2        1
016800050105     o                                         +  0 'Bolla che transita dal'
016900050105     o                                         +  0 ' P.O. in modifica'
017000050105     o                       BtPflp            +  1
017100050105     o                                         +  2 'Bolla'
017200050105     o                       BtPAAS            +  2
017300050105     o                       BtPLNP            +  1
017400050105     o                       BtPNRS        z   +  1
017500050105     o                       BtPNSP        2   +  1
017600050105     o                                         +  2 'Entrata al transito:'
017700050105     o                       BtPdet            +  1 '    /  /  '
017800050105     o               99                        +  2 'già presente x flp'
017900050105     o               99      partpn            +  1
018000050103     oQsysprt   e            totale      2
018100050105     o                                         +  0 'Totale FNBTP00F aggiornate'
018200050105     o          e            totale      2
018300050105     o                                         +  5 'Bolle deletate   con DET=0'
018400050105     o                                         +  1 'transito nuovo terminal'
018500050105     o                       wtotdelet1    2   +  2
018600050105     o          e            totale      2
018700050105     o                                         +  5 'Bolle aggiornate con DET>0'
018800050105     o                                         +  1 'transito nuovo terminal'
018900050105     o                       wtotagg1      2   +  2
019000050105     o          e            totale      2
019100050105     o                                         +  5 'Bolle deletate   con DET=0'
019200050105     o                                         +  1 'transito p.o. in modifica'
019300050105     o                       wtotdelet2    2   +  2
019400050105     o          e            totale      2
019500050105     o                                         +  5 'Bolle aggiornate con DET>0'
019600050105     o                                         +  1 'transito p.o. in modifica'
019700050105     o                       wtotagg2      2   +  2
019800050105     o          e            totale      2
019900050105     o                                         +  5 'Bolle deletate   con DET>0'
020000050105     o                                         +  1 'transito p.o. in modifica'
020100050105     o                                         +  1 'perchè già presenti con FL'
020200050105     o                                         +  0 'P=new terminal'
020300050105     o                       wtoterr2      2   +  2
020400050103** ovrp
020500050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
