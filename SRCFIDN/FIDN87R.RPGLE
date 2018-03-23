000100050720      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050720      *===============================================================*
000300051025      *?FIDN87R * Lista Eventi per richiesta recupero ad A.I.G.      ?*
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
006000051017     d   V1Cse5                       1    inz
006100050720      *
006200050720      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006300050720      *
006400050721      * - Campi di accesso al sfl
006500050721     d S01nrr          s                   like(C01rcd) inz
006600050914     d W1Cnev          s                   like(S1Cnev) inz
006700050919     d W1Caae          s                   like(V1Caae) inz
006800050914     d TOTipl          s                   like(S1Cipl) inz
006900050914     d TOTir1          s                   like(S1Cir1) inz
007000050914     d TOTiri          s                   like(S1Ciri) inz
007100050728      * - Campi definiti internamente
007200050728     d Wtime           s              6  0 inz
007300050919      * - Parametri per QCMDEXC
007400050919     d Qcmd            s             80    inz
007500050919     d Qlen            s             15  5 inz(80)
007600050720
007700050720      *---------------------------------------------------------------*
007800050720      *  RIEPILOGO INDICATORI                                         *
007900050720      *---------------------------------------------------------------*
008000050720      * 10    - Comodo                                                *
008100050919      * 41    - Condiziona stampa flag di eventi esclusi (DETFTR=E)   *
008200050720      *---------------------------------------------------------------*
008300050720
008400050720     c     *Entry        plist
008500050720     c                   parm                    KPJBA
008600050720      *
008700050721      * Reperimento dati del lavoro
008800050721     c                   exsr      DatiJob
008900050720      *
009000050720      * Stampa
009100050720     c                   exsr      Stampa
009200050720      *
009300050720      * Fine
009400050720     c                   clear                   TIBS02ds
009500050720     c                   eval      T02tla   = 'C'
009600050720     c                   call      'TIBS02R'
009700050720     c                   parm                    KPJBA
009800050720     c                   parm                    TIBS02ds
009900050720     c                   movel     *on           *inLR
010000050720      *
010100050720      *---------------------------------------------------------------*
010200050720      * Reperimento Dati del job (Utente/Operativi)                   *
010300050720      *---------------------------------------------------------------*
010400050720     c     DatiJob       BEGSR
010500050720      *
010600050720     c     *dtaara       define    §azute        azuteds
010700050720     c     *dtaara       define    §datiute      ddatiute
010800050720      *
010900050720     c                   in(E)     *dtaara
011000050720     c                   IF        %ERROR or RSUT = *blanks
011100050720     c                   clear                   Tibs34Ds
011200050720     c                   call      'TIBS34R'
011300050720     c                   parm                    Tibs34Ds
011400050720     c                   in        *dtaara
011500050720     c                   ENDIF
011600050720      *
011700050720     c                   ENDSR
011800050720      *
011900050720      *---------------------------------------------------------------*
012000050720      * Stampa elenco eventi                                          *
012100050720      *---------------------------------------------------------------*
012200050720     c     Stampa        BEGSR
012300050720      *
012400050919      * Override al prtf
012500050919     c                   call      'QCMDEXC'
012600050919     c                   parm      $Cmd(1)       Qcmd
012700050919     c                   parm                    Qlen
012800050919      *
012900050919      * Apertura prtf
013000050919     c                   open      PRTF198
013100050919      *
013200050919     c                   movel     KPJBU         FIDN86ds
013300050919      *
013400050914     c                   if        V1Cnev   > *zeros
013500050914     c                   move      V1Cnev        W1Cnev
013600050914     c                   z-add     V1Caae        W1Caae
013700050914     c                   endif
013800051013     c                   eval      *in41    = (V1Cse4 = 'N')
013900050919     c                   time                    Wtime
014000050914      * Stampa parametri di lancio (da D01)
014100050914     c                   except    PRTtxt1
014200050914     c                   except    PRTprm
014300050914      * Stampa testata
014400050914     c                   except    PRTtxt1
014500050914     c                   except    PRTtxt2
014600050720      *
014700050721      * Elaborazione sfl FIDN86D
014800050721if  1c                   DO        *hival        S01nrr
014900050721      *
015000050721     c     S01nrr        chain     FI86S01
015100050728if  2c                   if        NOT %found(FIDN86D)
015200050721     c                   leave
015300050721e   2c                   endif
015400050721      *
015500050720     c                   exsr      PrintDett
015600050721      *
015700051013if  2c                   if        S1Copz  <> *blanks
015800050721     c                   eval      *in32    = *on
015900050721     c                   UPDATE    FI86S01
016000050721e   2c                   endif
016100050720      *
016200051013e   1c                   ENDDO
016300050720      *
016400050720      * Stampa fine lista
016500050914if  1c                   if            TOTipl  = *zeros
016600050914     c                             and TOTir1  = *zeros
016700050914     c                             and TOTiri  = *zeros
016800050720     c                   except    PRTend
016900050914x   1c                   else
017000050914     c                   except    PRTtot
017100050914e   1c                   endif
017200050919      *
017300050919      * Chiusura prtf
017400050919     c                   close     PRTF198
017500050919      *
017600050919      * Cancellazione override al prtf
017700050919     c                   call      'QCMDEXC'
017800050919     c                   parm      $Cmd(2)       Qcmd
017900050919     c                   parm                    Qlen
018000050720      *
018100050720     c                   ENDSR
018200050720      *
018300050720      *---------------------------------------------------------------*
018400050720      * Stampa del record                                             *
018500050720      *---------------------------------------------------------------*
018600050720     c     PrintDett     BEGSR
018700050720      *
018800050721      * Decodifica Tipo Anomalia
018900050721     c                   exsr      DecodTAD
019000050720      * Decodifica P.O. che ha Comunicato l'Evento
019100050720     c                   exsr      DecodFCE
019200050720      *
019300050720      * Stampa
019400050720     c                   if        *inOF    = *on
019500050914     c                   except    PRTtxt1
019600050914     c                   except    PRTtxt2
019700050720     c                   eval      *inOF    = *off
019800050720     c                   endif
019900050720     c                   except    PRTdet
020000050720      *
020100050914      * Aggiornmento totali
020200050914     c                   add       S1Cipl        TOTipl
020300050914     c                   add       S1Cir1        TOTir1
020400050914     c                   add       S1Ciri        TOTiri
020500050720      *
020600050720     c                   ENDSR
020700050720      *
020800050720      *---------------------------------------------------------------*
020900050720      * Decodifica / Controllo TIPO ANOMALIA                          *
021000050720      *---------------------------------------------------------------*
021100050720     c     DecodTAD      BEGSR
021200050720      *
021300050720     c                   clear                   dTAD
021400050720     c                   reset                   TIBS02ds
021500050720     c                   movel     KNSIF         T02sif
021600050720     c*** già impostato: movel     'TAD'         T02cod
021700050721     c                   movel(p)  S1Ctad        T02ke1
021800050720     c                   call      'TIBS02R'
021900050720     c                   parm                    KPJBA
022000050720     c                   parm                    TIBS02ds
022100050720     c                   if        T02err   = *blanks
022200050720     c                   movel     T02uni        dTAD
022300050720     c                   endif
022400050720      *
022500050720     c                   ENDSR
022600050720      *
022700050720      *---------------------------------------------------------------*
022800050720      * Decodifica P.O. che ha Comunicato l'Evento                    *
022900050720      *---------------------------------------------------------------*
023000050720     c     DecodFCE      BEGSR
023100050720      *
023200050721     c     S1Cfce        chain     AZORG
023300050720     c                   if        NOT %found(AZORG01L)
023400050720     c                             or  ORGfva <> *blanks
023500050720     c                   clear                   ORGdes
023600050720     c                   endif
023700050720      *
023800050720     c                   ENDSR
023900050720
024000050720      *---------------------------------------------------------------*
024100050720
024200050914     oPRTF198   e            PRTtxt1           2
024300050720     o                       RSUT
024400051025     o                                          100 'LISTA EVENTI INTE-
024500051025     o                                              RESSATI AL RECUPER-
024600051025     o                                              O'
024700050721     o                       SDSpgm             140
024800050720     o                       *date         y  +   5
024900050720     o                       Wtime            +   5 '  :  :  '
025000050720     o                                        +   5 'Pag.'
025100050720     o                       Page             +   0
025200050914     o          e            PRTtxt1     0
025300051025     o                                          100 'LISTA EVENTI INTE-
025400051025     o                                              RESSATI AL RECUPER-
025500051025     o                                              O'
025600050914      *
025700050914     o          e            PRTprm     10
025800050914     o                                              'PARAMETRI DI LANC-
025900050914     o                                              IO:'
026000050914     o                                        +  10 'Numero Evento ...-
026100050914     o                                              ...:    '
026200050914     o                       W1Cnev        z  +   2
026300051021     o                                        +   9 'Anno:'
026400050914     o                       W1Caae        z  +   2
026500050914     o          e            PRTprm      0  0
026600050914     o                                              '_________________-
026700050914     o                                              ___'
026800050914     o          e            PRTprm      3
026900050914     o                                        +  30 'Data apertura Eve-
027000050914     o                                              nto: dal'
027100050914     o                       V1Cdai           +   2 '  /  /    '
027200050914     o                                        +   2 'al'
027300050914     o                       V1Cdaf           +   2 '  /  /    '
027400050914     o          e            PRTprm     10
027500051011     o                                        +  30 'SELEZIONE EVENTI:'
027600051011     o          e            PRTprm      0  0
027700051011     o                                        +  30 '_________________'
027800051011     o          e            PRTprm      3
027900051011     o                                        +  30 'Eventi con richie-
028000051025     o                                              sta di recupero da-
028100051011     o                                               eseguire:'
028200050914     o                       V1Cse1           +   1
028300051021     o                                        +   5 '(S=Sì, N=No)'
028400050914     o          e            PRTprm      3
028500051011     o                                        +  30 'Eventi in attesa -
028600051025     o                                              di recupero ......-
028700051011     o                                              .........:'
028800050914     o                       V1Cse2           +   1
028900051021     o                                        +   5 '(S=Sì, N=No)'
029000051011     o          e            PRTprm      3
029100051025     o                                        +  30 'Eventi recuperati-
029200051011     o                                               totalmente ......-
029300051011     o                                              .........:'
029400051011     o                       V1Cse3           +   1
029500051021     o                                        +   5 '(S=Sì, N=No)'
029600050914     o          e            PRTprm      3
029700051011     o                                        +  30 'Eventi esclusi ..-
029800051011     o                                              ..................-
029900051011     o                                              .........:'
030000051011     o                       V1Cse4           +   1
030100051021     o                                        +   5 '(S=Sì, N=No)'
030200051017     o          e            PRTprm      3
030300051017     o                                        +  30 'Eventi fortuiti .-
030400051017     o                                              ..................-
030500051017     o                                              .........:'
030600051017     o                       V1Cse5           +   1
030700051021     o                                        +   5 '(S=Sì, N=No)'
030800050914     o          e            PRTtxt2     2
030900050720     o                       $TXT(1)
031000050914     o                       $TXT(5)          +   0
031100050919     o               41                         198 '          '
031200050914     o          e            PRTtxt2     1
031300050720     o                       $TXT(2)
031400050914     o                       $TXT(6)          +   0
031500050919     o               41                         198 '          '
031600050914     o          e            PRTtxt2     1  1
031700050720     o                       $TXT(3)
031800050914     o                       $TXT(7)          +   0
031900050919     o               41                         198 '          '
032000050914      *
032100050720     o          e            PRTdet      1
032200050721     o                       S1Cnev        z
032300050721     o                       S1Cdev        y  +   2
032400051017     o                       S1Cevf           +   1
032500050721     o                       S1Ctad           +   3
032600050721     o                       §TADdesc         +   1
032700050721     o                       S1Dnot           +   3
032800050920     o                       S1Cfce           +   3
032900050720     o                       ORGdes           +   1
033000050802     o                       S1Cdrr           +   3 '  /  /    '
033100050802     o                       S1Cdvr           +   4 '  /  /    '
033200050721     o                       S1Cipl        k  +   4
033300050721     o                       S1Cir1        k  +   2
033400050802     o                       S1Ciri        k  +   2
033500050919     o              N41      S1Cfle           +   4
033600050914      *
033700050914     o          e            PRTtot      2
033800050914     o                       $TXT(9)
033900050914     o                       $TXT(10)         +   0
034000050914     o                       TOTipl        k    159
034100050914     o                       TOTir1        k  +   2
034200050914     o                       TOTiri        k  +   2
034300050720      *
034400050720     o          e            PRTend      2
034500050720     o                                           80 '***   Fine Lista   ***'
034600050720     o          e            PRTend      0
034700050720     o                                           80 '***   Fine Lista   ***'
034800050720
034900050720      *---------------------------------------------------------------*
035000050720
035100050919**   $Cmd ---------------------------------------------------------------------*
035200051025OVRPRTF FILE(PRTF198) USRDTA('RecuperoEv')
035300050919DLTOVR  FILE(PRTF198)
035400050720**   $TXT ....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....A
035500051017*   E V E N T O   *
035600051017 Numero     del   F   Tipo Anomalia                  Descrizione                           P.O. com
035700050914---------------------------------------------------------------------------------------------------
0358000510181234567  12/34/56 X   12 1234567890123456789012345   12345678901234567890123456789012345   123 1234
035900051025                   Data Ultima   Data Ultima      Liquidato       Recupero        Importo   Esclu
036000051025unicazione         Rich.Recup.   Ricez.Recup        Cliente      Richiesto     Recuperato   sione
036100050914-------------------------------------------------------------------------------------------------
0362000507215678901234567890   12/34/5678    12/34/5678    1.234.567,89-  1.234.567,89-  1.234.567,89-    1
036300050914                                                     T O T A L E   . . . . . . . . . . . . . . . .
036400050914. . . . . . . . . . . . . . . . . . . . . . .  1.234.567,89-  1.234.567,89-  1.234.567,89-
