000100131024     H DECEDIT('0,') DATEDIT(*DMY.)
000200131024      * ------------------------------------------------------------------
000300021211      *                                                                    *
000400131023      *--TRASFERIMENTO FIVAB46F/50f a filiale   + stampa dati trasmessi
000500001018      *
000600021211      * ------------------------------------------------------------------
000700131024     FFivab46f  uf   e             disk    usropn extfile(wFIVAB46)
000800131024     f                                     rename(fivab000:fivab046)
000900131024     FFivab00f  if a e             disk    usropn extfile(wfivab00)
001000131023     f
001100021211     FPRTF198   O    F  198        PRINTER OFLIND(*INOG)
001200021211     F                                     usropn
001300021211      *----------------
001400021211      * Schiere e Ds
001500021211      *----------------
001600021211     d KPJBA         e ds                                                       *trasmissione
001700131029     D CMD             S             85    DIM(1) CTDATA PERRCD(1)
001800131029     D CMD2            S             85    DIM(1) CTDATA PERRCD(1)
001900131024     d
002000131029     D* DS PER TRUL90R - RICHIESTA STAMPANTI
002100131029     D Trul90ds      e ds                  inz
002200131024     D                 DS
002300131024     D  vabaas                 1      4  0
002400131024     D  vabmgs                 5      8  0
002500131024     D  vabdsp                 1      8  0
002600131024     d
002700131023     d Datasys         s               d   datfmt(*iso)  inz(*sys)
002800131023     d k_key           s              3
002900131024     d winvia          s              4
003000131029     d wcmd            s              1
003100131024     d wfivab46        s             21    inz('GAITRAGRU /FIVAB46F')
003200131024     d wfivab00        s             21    inz('FILTRAGRU /FIVAB00F')
003300001018      *--------------------------------------------------------------------*
003400001018      *-                       M A I N    L I N E                         -*
003500001018      *--------------------------------------------------------------------*
003600021211      * Operazioni iniziali
003700001018      *
003800021211     C     *ENTRY        PLIST
003900021211     C                   PARM                    KPJBA
004000131029     c*
004100131023     c*  Verifico se data passata --> se non passata imposto udate
004200131029    1c                   if        %subst(kpjbu:1:8)>*zeros
004300131023     c                   movel     kpjbu         wdataA            8 0
004400131029
004500131029     c                   eval      wcmd='1'
004600131029
004700131029   x1c                   else
004800131029
004900131023     c                   movel     datasys       wdataA
005000131029     c                   eval      wcmd='2'
005100131029     c
005200131029     c* Se non passata prendo la stampante "tabutati" con il trul90r
005300131029     c                   eval      d90key=%subst(kpjbu:20:3)
005400131029    2c                   if        d90key=*blanks
005500131029     c                   eval      d90key='ACQ'
005600131029    2c                   endif
005700131029     c
005800131029     c                   clear                   trul90ds
005900131029     c                   eval      d90rst='X'
006000131029     C                   call      'TRUL90R'
006100131029     C                   PARM                    KPJBA
006200131029     C                   parm                    Trul90ds
006300131029     C                   parm                    d90key            3
006400131029
006500131029     c* trovata stampate : la imposto
006600131029    2c                   if        d90prt<>*blanks
006700131029     c                   eval      %subst(CMD2:72:10)=d90prt
006800131029   x2c                   else
006900131029     c                   eval      wcmd='1'
007000131029    2c                   endif
007100131029    1c                   endif
007200131023     c
007300131029     c* Verifico chi trasmette
007400131029     c                   eval      k_key=  %subst(kpjbu:20:3)
007500131029     c                   eval      winvia='ACQ '
007600131024     c* Apertura fivabxxF giusto di partenza
007700131029    1c                   if        k_key='SED'
007800131024     c                   eval      winvia='SEDE'
007900131025     c                   eval      %subst(CMD:38:4)='SEDE'
008000131029     c                   eval      %subst(CMD2:38:4)='SEDE'
008100131024     c                   eval      %subst(wfivab46:17:2)='50'
008200131029    1c                   endif
008300131024
008400131024     c                   open(e)   fivab46f
008500131029    1c                   if        %error
008600131024     c                   eval      %subst(wfivab46:7:4)='GRPS'
008700131024     c                   open(e)   fivab46f
008800131029    1c                   endif
008900131024
009000131024
009100131024     c* file di ricezione bolle di filiale
009200131024     c                   open(e)   fivab00f
009300131029    1c                   if        %error
009400131024     c                   eval      %subst(wfivab00:7:4)='GRPF'
009500131024     c                   open(e)   fivab00f
009600131029    1c                   endif
009700021211      *
009800021211      * reperisco ora del sistema
009900021211     C                   TIME                    W0140            14 0
010000021211     C                   MOVEL     W0140         TIME              6 0
010100021211     C                   z-add     *zeros        $nrr              5 0
010200021211      *
010300021211      * ovrprtf su file printer
010400131029    1c                   if        wcmd='1'
010500131029     C                   MOVEA     CMD           COMMAN           85
010600131029     c                   else
010700131029     C                   MOVEA     CMD2          COMMAN           85
010800131029    1c                   endif
010900131029     c
011000131029     C                   Z-ADD     85            LUNG             15 5
011100021211      *
011200021211     C                   CALL      'QCMDEXC'                              99
011300021211     C                   PARM                    COMMAN
011400021211     C                   PARM                    LUNG
011500021211      *
011600021211     C                   OPEN      PRTF198
011700131024     c
011800131024     c* lettura del file : elaboro solo le bolle con data < = alla data passata
011900131024     c     *start        setll     fivab46f
012000131024     c                   read      fivab46f
012100131024
012200131024     c                   dow       not %eof(fivab46f)
012300131024     c                   if        vabdsp<=wdataA
012400131024     C                   add       1             $Nrr
012500131024     c                   exsr      stampo
012600131024     c
012700131024     c                   write     fivab000
012800131024     c
012900131024     c                   delete    fivab046
013000131024     c                   endif
013100131024     c                   read      fivab46f
013200131024     c                   enddo
013300021211      * Chiudo/Invio
013400021211     C                   CLOSE     FiVab46f
013500131024     c
013600021211      * Stampo spedizioni non trovate
013700131025     c                   if        $nrr=0
013800021211     c   OG
013900021211     cORN77              DO
014000021211     c                   EXCEPT    TESTA
014100021211     c  N77              SETON                                        77
014200021211     c                   ENDDO
014300021211     c                   EXCEPT    DETTA1
014400021211     c                   ENDIF
014500131024
014600021211     c                   SETON                                        LR
014700021211      *
014800021211      * STAMPO DATI DA TRASMETTERE -----------------------------------*
014900021211     C     STAMPO        BEGSR
015000021211      *
015100021211     c   OG
015200021211     cORN77              DO
015300021211     c                   EXCEPT    TESTA
015400021211     c                   EXCEPT    TESTA1
015500021211     c                   EXCEPT    TESTA2
015600021211     c                   EXCEPT    TESTA3
015700021211     c  N77              SETON                                        77
015800021211     c                   ENDDO
015900021211      *
016000021211     c                   movel     vabrsd        Dest             20
016100021211     c                   movel     vabind        Ind              20
016200021211     c                   movel     vabcad        Cap               5
016300021211     c                   movel     vablod        Loc              20
016400021211     c                   EXCEPT    DETTA
016500021211     C                   ENDSR
016600021211      *
016700021211      ********************************************************************
016800021211     oPRTF198   E            TESTA          2 02
016900021211     o                       UDATE             +  2 '  /  /  '
017000021211     o                       TIME              +  2 '  :  :  '
017100021211     o                                         + 35 'Elenco spedizioni inviate '
017200131024     o                                         +  0 'da '
017300131024     o                       winvia
017400131024     o                                         +  0 ' a BOLOGNA Fil'
017500021211     o                                         +  0 'iale'
017600131023     o                                         + 10 'TNSD57R'
017700021211     o                                         +  5 'PAG.'
017800021211     o                       PAGE          Z
017900021211     o          E            TESTA1         2
018000021211     o                                         +  0 '  MM. Nr   Num.    Cd Fil '
018100021211     o                                         +  0 'Destinatario         Indir'
018200021211     o                                         +  0 'izzo            C A P Loca'
018300021211     o                                         +  0 'lità            Prv E Coll'
018400021211     o                                         +  0 'li       Peso             '
018500021211     o                                         +  0 '   Riferim. Segnac. Segnac'
018600021211     o                                         +  0 '. Zo F Descrizione'
018700021211     o          E            TESTA2         1
018800021211     o                                         +  0 '  GG. Sr   Sped.   Bl Arr '
018900021211     o                                         +  0 '                          '
019000021211     o                                         +  0 '                          '
019100021211     o                                         +  0 '                    s     '
019200021211     o                                         +  0 '         KG               '
019300021211     o                                         +  0 '   Mittente DAL     AL    '
019400021211     o                                         +  0 '  na D  N O T E'
019500021211     o          E            TESTA3         1
019600021211     o                                         +  0 '                   Pa     '
019700021211     o                                         +  0 '                          '
019800021211     o                                         +  0 '                          '
019900021211     o                                         +  0 '                    p     '
020000021211     o                                         + 14 '               Numerico'
020100021211     o          E            DETTA          2
020200021211     o                       VABMGS            +  0 '  /  '
020300021211     o                       VABNRS            +  1
020400021211     o                       VABNSP        2   +  1
020500021211     o                       VABCBO            +  1
020600021211     o                       VABLNA            +  1
020700021211     o                       DEST              +  1
020800021211     o                       IND               +  1
020900021211     o                       CAP               +  1
021000021211     o                       LOC               +  1
021100021211     o                       VABPRD            +  1
021200021211     o                       VABTSP            +  1
021300021211     o                       VABNCL        2   +  1
021400021211     o                       VABPKB        2   +  1
021500021211     o                       VABRMN        2   +  1
021600021211     o                       VABNCD        2   +  2
021700021211     o                       VABNCA        2   +  1
021800021211     o                       VABZNC        2   +  1
021900021211     o                       VABFFD            +  1
022000021211     o                       VABNOT            +  1
022100021211     o          E            DETTA1           06
022200021211     o                                         + 65 '--- NON TROVATE SPE'
022300021211     o                                         +  0 'DIZIONI ---'
022400021211** Comandi    567890123456789012345678901234567890123456789012345678901234567890
022500131025OVRPRTF FILE(PRTF198)  USRDTA('Bolle-ACQ ') HOLD(*YES) SAVE(*YES)
022600131029** Comandi    567890123456789012345678901234567890123456789012345678901234567890
022700131029OVRPRTF FILE(PRTF198)  USRDTA('Bolle-ACQ ') HOLD(*YES) SAVE(*YES) outq(          )
