000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200050105      * TIBS82R2 *----------------------------------------------------*
000300050105      *  - MODIFICA PUNTO OPERATIVO: sistemo bolle transito    BTP
000400940915      *--------------------------------------------------------------*
000500050105     Ffnbtp15l  UF   E           K DISK
000600050105     FfnbtT11l  UF   E           K DISK
000700060102     Fazorg01l  iF   E           K DISK
000800050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000900960302     D*
001000050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001100050105     d kflp            s                   Like(btpflp)
001200050105     d kdut            s                   Like(btpdut)
001300050105     d waggiorna       s              1
001400050105     d Dataiso         s               d   datfmt(*iso)
001500040917     C**********************************************************************
001600050105     c     kbtp          KLIST
001700050105     c                   KFLD                    kflp
001800050105     c                   KFLD                    kdut
001900050105     c     kbtt          KLIST
002000050105     c                   KFLD                    BtPflp
002100050105     c                   KFLD                    BtPAAS
002200050105     c                   KFLD                    BtPLNP
002300050105     c                   KFLD                    BtPNRS
002400050105     c                   KFLD                    BtPNSP
002500050103     C**********************************************************************
002600040917     C     *ENTRY        PLIST
002700050103     C                   PARM                    parpo             3 0
002800050103     C                   PARM                    partpo            3 0
002900050103     C                   PARM                    partpn            3 0
003000050105     c* Giro  udate
003100050105     c                   Move      *date         Dataiso
003200050105     c                   Move      dataiso       dateu             8 0
003300050103     c
003400050103     c                   eval      comman=ovrp(1)
003500050103     C                   CALL      'QCMDEXC'
003600050103     C                   PARM                    COMMAN           80
003700050103     C                   PARM      80            LUNG             15 5
003800050103     c
003900050103     c                   open      qsysprt
004000060102     c
004100060102     c     parpo         chain     azorg01l
004200060102     c
004300060102     c                   if        %found(azorg01l)
004400060102     c                   movel     orgdes        despo            25
004500060102     c                   else
004600060102     c                   clear                   despo
004700060102     c                   endif
004800060102     c
004900050103     c
005000050105     c* Leggo tutte le bolle transito con btpflp=new terminal e dut=0
005100050105     c                   eval      kflp=partpn
005200050105     c                   eval      kdut=0
005300050105     c
005400050105     c     kbtp          setll     fnbtp15l
005500050105     c     kbtp          reade     fnbtp15l
005600050105    1c                   dow       not %eof(fnbtp15l)
005700050103     c                   clear                   welabora          1
005800050103    2c                   if        parpo=partpo
005900050105    3c                   if        btptfp=parpo
006000050103     c                   eval      welabora='S'
006100050103    3c                   endif
006200050103     c
006300050103   X2c                   else
006400050103     c* altrimenti elaboro solo la linea
006500050105    3c                   if        btplnp=parpo
006600050103     c                   eval      welabora='S'
006700050103    3c                   endif
006800050103    2c                   endif
006900050103     c
007000050103    2c                   if        welabora='S'
007100050105     c* Se det =0 stampo e deleto
007200050105    3c                   if        btpdet=0
007300050105     c                   except    btperr1
007400050105     c
007500050105     c* D=deleta record
007600050105     c                   eval      waggiorna='D'
007700050105     c                   exsr      aggbtt
007800050105     c                   delete    fnbtp000
007900050105     c                   add       1             wtotdelet1        7 0
008000050105   x3c                   else
008100050105     c                   except    btperr1
008200050105     c* C=chiudi record
008300050105     c                   eval      waggiorna='C'
008400050105     c                   exsr      aggbtt
008500050105     c                   eval      btpdut=dateu
008600050105     c                   update    fnbtp000
008700050105     c                   add       1             wtotagg1          7 0
008800050105     c
008900050103    3c                   endif
009000050103    2c                   endif
009100050105     c
009200050105     c     kbtp          reade     fnbtp15l
009300050105    1C                   ENDDO
009400050105     c
009500050105     c* Leggo tutte le bolle transito con btpflp=old p.o.   e dut=0
009600050105     c                   eval      kflp=parpo
009700050105     c                   eval      kdut=0
009800050105     c
009900050105     c     kbtp          setll     fnbtp15l
010000050105     c     kbtp          reade     fnbtp15l
010100050105    1c                   dow       not %eof(fnbtp15l)
010200050105     c                   setoff                                       99
010300050105     c
010400050105     c* Se det =0 stampo e deleto
010500050105    2c                   if        btpdet=0
010600050105     c                   except    btperr2
010700050105     c
010800050105     c* D=deleta record
010900050105     c                   eval      waggiorna='D'
011000050105     c                   exsr      aggbtt
011100050105     c                   delete    fnbtp000
011200050105     c                   add       1             wtotdelet2        7 0
011300050105   x2c                   else
011400050105     c                   except    btperr2
011500050105     c* M=modifica p.o. transito nel nuovo terminal
011600050105     c                   eval      waggiorna='M'
011700050105     c                   exsr      aggbtt
011800050105     c                   eval      btpflp=partpn
011900050105     c                   update    fnbtp000                             99
012000050105    3c                   if        *in99
012100050105     c                   except    btperr2
012200050105     c                   add       1             wtoterr2          7 0
012300050105     c                   delete    fnbtp000
012400050105     c                   else
012500050105     c                   add       1             wtotagg2          7 0
012600050105    3c                   endif
012700050105     c
012800050105    2c                   endif
012900050105     c
013000050105     c     kbtp          reade     fnbtp15l
013100050105    1C                   ENDDO
013200050103     c
013300050103     c* Stampo totale bolle aggiornate
013400050103     c                   except    totale
013500050103     c
013600050103     c                   seton                                        lr
013700000828      *
013800951025     C**************************************************************************
013900951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
014000951025     C**************************************************************************
014100050105     C     aggbtt        BEGSR
014200020213
014300050105     C     kbtt          setll     fnbtt11l
014400050105     C     kbtt          reade     fnbtt11l
014500050105     c                   dow       not %eof(fnbtt11l)
014600050105     c                   select
014700050105     c                   when      waggiorna='D'
014800050105     c                   delete    fnbtt000
014900050105
015000050105     c                   when      waggiorna='C'
015100050105     c                   eval      bttdut=dateu
015200050105     c                   update    fnbtt000
015300050105
015400050105     c                   when      waggiorna='M'
015500050105     c                   eval      bttflp=partpn
015600050105     c                   update    fnbtt000                             99
015700050105     c                   if        *in99
015800050105     c                   delete    fnbtt000
015900050104     c                   endif
016000050105     c                   endsl
016100050105     c
016200050105     C     kbtt          reade     fnbtt11l
016300050103     c                   enddo
016400050103     c
016500951025     C                   ENDSR
016600050105     oQsysprt   e            BTPERR1        1
016700050105     o                                         +  0 'Bolla che transita dal '
016800050105     o                                         +  0 'nuovo terminal partenza:'
016900050105     o                       BtPflp            +  1
017000050105     o                                         +  2 'Bolla'
017100050105     o                       BtPAAS            +  2
017200050105     o                       BtPLNP            +  1
017300050105     o                       BtPNRS        z   +  1
017400050105     o                       BtPNSP        2   +  1
017500050105     o                                         +  2 'Entrata al transito:'
017600050105     o                       BtPdet            +  1 '    /  /  '
017700050105     oQsysprt   e            BTPERR2        1
017800050105     o                                         +  0 'Bolla che transita dal'
017900050105     o                                         +  0 ' P.O. in modifica'
018000050105     o                       BtPflp            +  1
018100050105     o                                         +  2 'Bolla'
018200050105     o                       BtPAAS            +  2
018300050105     o                       BtPLNP            +  1
018400050105     o                       BtPNRS        z   +  1
018500050105     o                       BtPNSP        2   +  1
018600050105     o                                         +  2 'Entrata al transito:'
018700050105     o                       BtPdet            +  1 '    /  /  '
018800050105     o               99                        +  2 'già presente x flp'
018900050105     o               99      partpn            +  1
019000060102     oQsysprt   e            totale      2
019100060102     o                                         +  0 'Punto Operativo Modificato'
019200060102     o                       parpo             +  2
019300060102     o                       despo             +  2
019400060102     o                       *date             + 25 '  /  /    '
019500050103     oQsysprt   e            totale      2
019600050105     o                                         +  0 'Totale FNBTP00F aggiornate'
019700050105     o          e            totale      2
019800050105     o                                         +  5 'Bolle deletate   con DET=0'
019900050105     o                                         +  1 'transito nuovo terminal'
020000050105     o                       wtotdelet1    2   +  2
020100050105     o          e            totale      2
020200050105     o                                         +  5 'Bolle aggiornate con DET>0'
020300050105     o                                         +  1 'transito nuovo terminal'
020400050105     o                       wtotagg1      2   +  2
020500050105     o          e            totale      2
020600050105     o                                         +  5 'Bolle deletate   con DET=0'
020700050105     o                                         +  1 'transito p.o. in modifica'
020800050105     o                       wtotdelet2    2   +  2
020900050105     o          e            totale      2
021000050105     o                                         +  5 'Bolle aggiornate con DET>0'
021100050105     o                                         +  1 'transito p.o. in modifica'
021200050105     o                       wtotagg2      2   +  2
021300050105     o          e            totale      2
021400050105     o                                         +  5 'Bolle deletate   con DET>0'
021500050105     o                                         +  1 'transito p.o. in modifica'
021600050105     o                                         +  1 'perchè già presenti con FL'
021700050105     o                                         +  0 'P=new terminal'
021800050105     o                       wtoterr2      2   +  2
021900050103** ovrp
022000050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
