000100050720      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050720      *===============================================================*
000300050921      *?FIDN87R * Lista Eventi per richiesta rimborso ad A.I.G.      ?*
000400050720      *===============================================================*
000500050720
000600050720     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050720
000800050720      *---------------------------------------------------------------*
000900050720
001000050721     fFIDN86D   cf   e             workstn
001100050728     f                                     include(FI86S01)
001200050728     f                                     include(FI86C01)
001300050721     f                                     sfile(FI86S01:S01nrr)
001400050720      *
001500050720     fAZORG01L  if   e           k disk
001600050720      *
001700050720     fPRTF198   o    f  198        printer oflind(*inOF)
001800050919     f                                     usropn
001900050720
002000050720      *---------------------------------------------------------------*
002100050720
002200050720      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002300050720      *
002400050919      * - Comandi da eseguire
002500050919     d $Cmd            s                   like(Qcmd)
002600050919     d                                     dim( 2) ctdata perrcd(1)
002700050720      * - Testate (con esempio di stampa)
002800050914     d $Txt            s             99    dim(10) ctdata perrcd(1)
002900050720      *
003000050720      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003100050720      *
003200050720     d KPJBA         e ds
003300050720      *
003400050720      * - Parametri x Controllo profilo utenti
003500050720     d TIBS34DS      e ds                  inz
003600050720      * - Ds di riferimento al file esterno AZUTE00F
003700050720     d AZUTEds       e ds                  extname(AZUTE00F)
003800050720      * - Ds per dati organigramma
003900050720     d DDatiUte      e ds
004000050721      *
004100050720      * - Parametri x Interrogazione tabelle
004200050720     d TIBS02DS      e ds                  inz
004300050720     d   T02mod      e                     inz('C')
004400050720     d   T02cod      e                     inz('TAD')
004500050720      * - Tab. "TAD"
004600050720     d dTAD          e ds                  inz
004700050720      *
004800050720     d Status         sds           333
004900050720     d   SDSpgm          *proc
005000050919      *
005100050919     d FIDN86ds        ds                  inz
005200050919     d   V1Cnev                       7    inz
005300050919     d   V1Caae                       4s 0 inz
005400050919     d   V1Cdai                       8s 0 inz
005500050919     d   V1Cdaf                       8s 0 inz
005600050919     d   V1Cse1                       1    inz
005700050919     d   V1Cse2                       1    inz
005800050919     d   V1Cse3                       1    inz
005900051011     d   V1Cse4                       1    inz
006000050720      *
006100050720      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006200050720      *
006300050721      * - Campi di accesso al sfl
006400050721     d S01nrr          s                   like(C01rcd) inz
006500050914     d W1Cnev          s                   like(S1Cnev) inz
006600050919     d W1Caae          s                   like(V1Caae) inz
006700050914     d TOTipl          s                   like(S1Cipl) inz
006800050914     d TOTir1          s                   like(S1Cir1) inz
006900050914     d TOTiri          s                   like(S1Ciri) inz
007000050728      * - Campi definiti internamente
007100050728     d Wtime           s              6  0 inz
007200050919      * - Parametri per QCMDEXC
007300050919     d Qcmd            s             80    inz
007400050919     d Qlen            s             15  5 inz(80)
007500050720
007600050720      *---------------------------------------------------------------*
007700050720      *  RIEPILOGO INDICATORI                                         *
007800050720      *---------------------------------------------------------------*
007900050720      * 10    - Comodo                                                *
008000050919      * 41    - Condiziona stampa flag di eventi esclusi (DETFTR=E)   *
008100050720      *---------------------------------------------------------------*
008200050720
008300050720     c     *Entry        plist
008400050720     c                   parm                    KPJBA
008500050720      *
008600050721      * Reperimento dati del lavoro
008700050721     c                   exsr      DatiJob
008800050720      *
008900050720      * Stampa
009000050720     c                   exsr      Stampa
009100050720      *
009200050720      * Fine
009300050720     c                   clear                   TIBS02ds
009400050720     c                   eval      T02tla   = 'C'
009500050720     c                   call      'TIBS02R'
009600050720     c                   parm                    KPJBA
009700050720     c                   parm                    TIBS02ds
009800050720     c                   movel     *on           *inLR
009900050720      *
010000050720      *---------------------------------------------------------------*
010100050720      * Reperimento Dati del job (Utente/Operativi)                   *
010200050720      *---------------------------------------------------------------*
010300050720     c     DatiJob       BEGSR
010400050720      *
010500050720     c     *dtaara       define    §azute        azuteds
010600050720     c     *dtaara       define    §datiute      ddatiute
010700050720      *
010800050720     c                   in(E)     *dtaara
010900050720     c                   IF        %ERROR or RSUT = *blanks
011000050720     c                   clear                   Tibs34Ds
011100050720     c                   call      'TIBS34R'
011200050720     c                   parm                    Tibs34Ds
011300050720     c                   in        *dtaara
011400050720     c                   ENDIF
011500050720      *
011600050720     c                   ENDSR
011700050720      *
011800050720      *---------------------------------------------------------------*
011900050720      * Stampa elenco eventi                                          *
012000050720      *---------------------------------------------------------------*
012100050720     c     Stampa        BEGSR
012200050720      *
012300050919      * Override al prtf
012400050919     c                   call      'QCMDEXC'
012500050919     c                   parm      $Cmd(1)       Qcmd
012600050919     c                   parm                    Qlen
012700050919      *
012800050919      * Apertura prtf
012900050919     c                   open      PRTF198
013000050919      *
013100050919     c                   movel     KPJBU         FIDN86ds
013200050919      *
013300050914     c                   if        V1Cnev   > *zeros
013400050914     c                   move      V1Cnev        W1Cnev
013500050914     c                   z-add     V1Caae        W1Caae
013600050914     c                   endif
013700051013     c                   eval      *in41    = (V1Cse4 = 'N')
013800050919     c                   time                    Wtime
013900050914      * Stampa parametri di lancio (da D01)
014000050914     c                   except    PRTtxt1
014100050914     c                   except    PRTprm
014200050914      * Stampa testata
014300050914     c                   except    PRTtxt1
014400050914     c                   except    PRTtxt2
014500050720      *
014600050721      * Elaborazione sfl FIDN86D
014700050721if  1c                   DO        *hival        S01nrr
014800050721      *
014900050721     c     S01nrr        chain     FI86S01
015000050728if  2c                   if        NOT %found(FIDN86D)
015100050721     c                   leave
015200050721e   2c                   endif
015300050721      *
015400050720     c                   exsr      PrintDett
015500050721      *
015600051013if  2c                   if        S1Copz  <> *blanks
015700050721     c                   eval      *in32    = *on
015800050721     c                   UPDATE    FI86S01
015900050721e   2c                   endif
016000050720      *
016100051013e   1c                   ENDDO
016200050720      *
016300050720      * Stampa fine lista
016400050914if  1c                   if            TOTipl  = *zeros
016500050914     c                             and TOTir1  = *zeros
016600050914     c                             and TOTiri  = *zeros
016700050720     c                   except    PRTend
016800050914x   1c                   else
016900050914     c                   except    PRTtot
017000050914e   1c                   endif
017100050919      *
017200050919      * Chiusura prtf
017300050919     c                   close     PRTF198
017400050919      *
017500050919      * Cancellazione override al prtf
017600050919     c                   call      'QCMDEXC'
017700050919     c                   parm      $Cmd(2)       Qcmd
017800050919     c                   parm                    Qlen
017900050720      *
018000050720     c                   ENDSR
018100050720      *
018200050720      *---------------------------------------------------------------*
018300050720      * Stampa del record                                             *
018400050720      *---------------------------------------------------------------*
018500050720     c     PrintDett     BEGSR
018600050720      *
018700050721      * Decodifica Tipo Anomalia
018800050721     c                   exsr      DecodTAD
018900050720      * Decodifica P.O. che ha Comunicato l'Evento
019000050720     c                   exsr      DecodFCE
019100050720      *
019200050720      * Stampa
019300050720     c                   if        *inOF    = *on
019400050914     c                   except    PRTtxt1
019500050914     c                   except    PRTtxt2
019600050720     c                   eval      *inOF    = *off
019700050720     c                   endif
019800050720     c                   except    PRTdet
019900050720      *
020000050914      * Aggiornmento totali
020100050914     c                   add       S1Cipl        TOTipl
020200050914     c                   add       S1Cir1        TOTir1
020300050914     c                   add       S1Ciri        TOTiri
020400050720      *
020500050720     c                   ENDSR
020600050720      *
020700050720      *---------------------------------------------------------------*
020800050720      * Decodifica / Controllo TIPO ANOMALIA                          *
020900050720      *---------------------------------------------------------------*
021000050720     c     DecodTAD      BEGSR
021100050720      *
021200050720     c                   clear                   dTAD
021300050720     c                   reset                   TIBS02ds
021400050720     c                   movel     KNSIF         T02sif
021500050720     c*** già impostato: movel     'TAD'         T02cod
021600050721     c                   movel(p)  S1Ctad        T02ke1
021700050720     c                   call      'TIBS02R'
021800050720     c                   parm                    KPJBA
021900050720     c                   parm                    TIBS02ds
022000050720     c                   if        T02err   = *blanks
022100050720     c                   movel     T02uni        dTAD
022200050720     c                   endif
022300050720      *
022400050720     c                   ENDSR
022500050720      *
022600050720      *---------------------------------------------------------------*
022700050720      * Decodifica P.O. che ha Comunicato l'Evento                    *
022800050720      *---------------------------------------------------------------*
022900050720     c     DecodFCE      BEGSR
023000050720      *
023100050721     c     S1Cfce        chain     AZORG
023200050720     c                   if        NOT %found(AZORG01L)
023300050720     c                             or  ORGfva <> *blanks
023400050720     c                   clear                   ORGdes
023500050720     c                   endif
023600050720      *
023700050720     c                   ENDSR
023800050720
023900050720      *---------------------------------------------------------------*
024000050720
024100050914     oPRTF198   e            PRTtxt1           2
024200050720     o                       RSUT
024300051011     o                                          105 'LISTA EVENTI NON -
024400050914     o                                              FORTUITI PER RICHI-
024500051011     o                                              ESTA RIMBORSO AD A-
024600051011     o                                              .I.G.'
024700050721     o                       SDSpgm             140
024800050720     o                       *date         y  +   5
024900050720     o                       Wtime            +   5 '  :  :  '
025000050720     o                                        +   5 'Pag.'
025100050720     o                       Page             +   0
025200050914     o          e            PRTtxt1     0
025300051011     o                                          105 'LISTA EVENTI NON -
025400050914     o                                              FORTUITI PER RICHI-
025500050914     o                                              ESTA RIMBORSO AD A-
025600051011     o                                              .I.G.'
025700050914      *
025800050914     o          e            PRTprm     10
025900050914     o                                              'PARAMETRI DI LANC-
026000050914     o                                              IO:'
026100050914     o                                        +  10 'Numero Evento ...-
026200050914     o                                              ...:    '
026300050914     o                       W1Cnev        z  +   2
026400050914     o                                        +   8 'Anno:'
026500050914     o                       W1Caae        z  +   2
026600050914     o          e            PRTprm      0  0
026700050914     o                                              '_________________-
026800050914     o                                              ___'
026900050914     o          e            PRTprm      3
027000050914     o                                        +  30 'Data apertura Eve-
027100050914     o                                              nto: dal'
027200050914     o                       V1Cdai           +   2 '  /  /    '
027300050914     o                                        +   2 'al'
027400050914     o                       V1Cdaf           +   2 '  /  /    '
027500050914     o          e            PRTprm     10
027600051011     o                                        +  30 'SELEZIONE EVENTI:'
027700051011     o          e            PRTprm      0  0
027800051011     o                                        +  30 '_________________'
027900051011     o          e            PRTprm      3
028000051011     o                                        +  30 'Eventi con richie-
028100051011     o                                              sta di rimborso da-
028200051011     o                                               eseguire:'
028300050914     o                       V1Cse1           +   1
028400051011     o                                        +   5 '(S=Sì, N=No)'
028500050914     o          e            PRTprm      3
028600051011     o                                        +  30 'Eventi in attesa -
028700051011     o                                              di rimborso ......-
028800051011     o                                              .........:'
028900050914     o                       V1Cse2           +   1
029000051011     o                                        +   5 '(S=Sì, N=No)'
029100051011     o          e            PRTprm      3
029200051011     o                                        +  30 'Eventi rimborsati-
029300051011     o                                               totalmente ......-
029400051011     o                                              .........:'
029500051011     o                       V1Cse3           +   1
029600051011     o                                        +   5 '(S=Sì, N=No)'
029700050914     o          e            PRTprm      3
029800051011     o                                        +  30 'Eventi esclusi ..-
029900051011     o                                              ..................-
030000051011     o                                              .........:'
030100051011     o                       V1Cse4           +   1
030200051011     o                                        +   5 '(S=Sì, N=No)'
030300050914     o          e            PRTtxt2     2
030400050720     o                       $TXT(1)
030500050914     o                       $TXT(5)          +   0
030600050919     o               41                         198 '          '
030700050914     o          e            PRTtxt2     1
030800050720     o                       $TXT(2)
030900050914     o                       $TXT(6)          +   0
031000050919     o               41                         198 '          '
031100050914     o          e            PRTtxt2     1  1
031200050720     o                       $TXT(3)
031300050914     o                       $TXT(7)          +   0
031400050919     o               41                         198 '          '
031500050914      *
031600050720     o          e            PRTdet      1
031700050721     o                       S1Cnev        z
031800050721     o                       S1Cdev        y  +   2
031900050721     o                       S1Ctad           +   3
032000050721     o                       §TADdesc         +   1
032100050721     o                       S1Dnot           +   3
032200050920     o                       S1Cfce           +   3
032300050720     o                       ORGdes           +   1
032400050802     o                       S1Cdrr           +   3 '  /  /    '
032500050802     o                       S1Cdvr           +   4 '  /  /    '
032600050721     o                       S1Cipl        k  +   4
032700050721     o                       S1Cir1        k  +   2
032800050802     o                       S1Ciri        k  +   2
032900050919     o              N41      S1Cfle           +   4
033000050914      *
033100050914     o          e            PRTtot      2
033200050914     o                       $TXT(9)
033300050914     o                       $TXT(10)         +   0
033400050914     o                       TOTipl        k    159
033500050914     o                       TOTir1        k  +   2
033600050914     o                       TOTiri        k  +   2
033700050720      *
033800050720     o          e            PRTend      2
033900050720     o                                           80 '***   Fine Lista   ***'
034000050720     o          e            PRTend      0
034100050720     o                                           80 '***   Fine Lista   ***'
034200050720
034300050720      *---------------------------------------------------------------*
034400050720
034500050919**   $Cmd ---------------------------------------------------------------------*
034600050919OVRPRTF FILE(PRTF198) USRDTA('Eventi AIG')
034700050919DLTOVR  FILE(PRTF198)
034800050720**   $TXT ....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....A
034900050720*   E V E N T O   *
035000050914 Numero      del      Tipo Anomalia                  Descrizione                           P.O. com
035100050914---------------------------------------------------------------------------------------------------
0352000507211234567  12/34/5678   12 1234567890123456789012345   12345678901234567890123456789012345   123 1234
035300050721                   Data Ultima   Data Ultima   Tot. Importi   Tot. Importi   Tot. Importi   Esclu
035400050721unicazione         Rich.Rimb.    Ricez.Rimb.      Liquidati      Richiesti     Rimborsati   sione
035500050914-------------------------------------------------------------------------------------------------
0356000507215678901234567890   12/34/5678    12/34/5678    1.234.567,89-  1.234.567,89-  1.234.567,89-    1
035700050914                                                     T O T A L E   . . . . . . . . . . . . . . . .
035800050914. . . . . . . . . . . . . . . . . . . . . . .  1.234.567,89-  1.234.567,89-  1.234.567,89-
