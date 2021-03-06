000100131024     H DECEDIT('0,') DATEDIT(*DMY.)
000200131024      * ------------------------------------------------------------------
000300021211      *                                                                    *
000400131023      *--TRASFERIMENTO FIVAB46F/50f a filiale   + stampa dati trasmessi
000500001018      *
000600021211      * ------------------------------------------------------------------
000700140107     FFivab46l  uf   e           k disk    usropn extfile(wFIVAB46)
000800131024     f                                     rename(fivab000:fivab046)
000900140121     FFivax51l  uf   e           k disk    usropn extfile(wFIVAx50)
001000140121     f                                     rename(fivax000:fivax050)
001100131024     FFivab00f  if a e             disk    usropn extfile(wfivab00)
001200140121     FFivax01l  if a e             disk    usropn extfile(wfivax00)
001300131023     f
001400021211     FPRTF198   O    F  198        PRINTER OFLIND(*INOG)
001500021211     F                                     usropn
001600021211      *----------------
001700021211      * Schiere e Ds
001800021211      *----------------
001900021211     d KPJBA         e ds                                                       *trasmissione
002000131029     D CMD             S             85    DIM(1) CTDATA PERRCD(1)
002100131029     D CMD2            S             85    DIM(1) CTDATA PERRCD(1)
002200131024     d
002300131029     D* DS PER TRUL90R - RICHIESTA STAMPANTI
002400131029     D Trul90ds      e ds                  inz
002500131024     D                 DS
002600131024     D  vabaas                 1      4  0
002700131024     D  vabmgs                 5      8  0
002800131024     D  vabdsp                 1      8  0
002900131024     d
003000131023     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003100131023     d k_key           s              3
003200131024     d winvia          s              4
003300131029     d wcmd            s              1
003400140107     d wfivab46        s             21    inz('GAITRAGRU /FIVAB46L')
003500140121     d wfivax50        s             21    inz('GAITRAGRU /FIVAX51L')
003600131024     d wfivab00        s             21    inz('FILTRAGRU /FIVAB00F')
003700140121     d wfivax00        s             21    inz('FILTRAGRU /FIVAX01L')
003800001018      *--------------------------------------------------------------------*
003900001018      *-                       M A I N    L I N E                         -*
004000001018      *--------------------------------------------------------------------*
004100021211      * Operazioni iniziali
004200001018      *
004300140121     c     kvab          klist
004400140121     c                   kfld                    vabfgs
004500140121     c                   kfld                    vabccm
004600140121     c                   kfld                    vabaas
004700140121     c                   kfld                    vablnp
004800140121     c                   kfld                    vabnrs
004900140121     c                   kfld                    vabnsp
005000140121      *--------------------------------------------------------------------*
005100021211     C     *ENTRY        PLIST
005200021211     C                   PARM                    KPJBA
005300131029     c*
005400131023     c*  Verifico se data passata --> se non passata imposto udate
005500131029    1c                   if        %subst(kpjbu:1:8)>*zeros
005600131023     c                   movel     kpjbu         wdataA            8 0
005700131029
005800131029     c                   eval      wcmd='1'
005900131029
006000131029   x1c                   else
006100131029
006200131023     c                   movel     datasys       wdataA
006300131029     c                   eval      wcmd='2'
006400131029     c
006500131029     c* Se non passata prendo la stampante "tabutati" con il trul90r
006600131029     c                   eval      d90key=%subst(kpjbu:20:3)
006700131029    2c                   if        d90key=*blanks
006800131029     c                   eval      d90key='ACQ'
006900131029    2c                   endif
007000131029     c
007100131029     c                   clear                   trul90ds
007200131029     c                   eval      d90rst='X'
007300131029     C                   call      'TRUL90R'
007400131029     C                   PARM                    KPJBA
007500131029     C                   parm                    Trul90ds
007600131029     C                   parm                    d90key            3
007700131029
007800131029     c* trovata stampate : la imposto
007900131029    2c                   if        d90prt<>*blanks
008000131029     c                   eval      %subst(CMD2:72:10)=d90prt
008100131029   x2c                   else
008200131029     c                   eval      wcmd='1'
008300131029    2c                   endif
008400131029    1c                   endif
008500131023     c
008600131029     c* Verifico chi trasmette
008700131029     c                   eval      k_key=  %subst(kpjbu:20:3)
008800131029     c                   eval      winvia='ACQ '
008900131024     c* Apertura fivabxxF giusto di partenza
009000131029    1c                   if        k_key='SED'
009100131024     c                   eval      winvia='SEDE'
009200131025     c                   eval      %subst(CMD:38:4)='SEDE'
009300131029     c                   eval      %subst(CMD2:38:4)='SEDE'
009400131024     c                   eval      %subst(wfivab46:17:2)='50'
009500131029    1c                   endif
009600131024
009700140107     c                   open(e)   fivab46l
009800131029    1c                   if        %error
009900131024     c                   eval      %subst(wfivab46:7:4)='GRPS'
010000140121     c                   eval      %subst(wfivax50:7:4)='GRPS'
010100140121     c                   open      fivab46l
010200131029    1c                   endif
010300140121
010400140121     c* solo per SEDE presente il fivax
010500140121    1c                   if        k_key='SED'
010600140121     c                   open      fivax51l
010700140121     c                   endif
010800131024
010900131024
011000131024     c* file di ricezione bolle di filiale
011100131024     c                   open(e)   fivab00f
011200131029    1c                   if        %error
011300131024     c                   eval      %subst(wfivab00:7:4)='GRPF'
011400140121     c                   eval      %subst(wfivax00:7:4)='GRPF'
011500140121     c                   open      fivab00f
011600131029    1c                   endif
011700140121     c* solo per SEDE presente il fivax
011800140121    1c                   if        k_key='SED'
011900140121     c                   open      fivax01l
012000140121     c                   endif
012100021211      *
012200021211      * reperisco ora del sistema
012300021211     C                   TIME                    W0140            14 0
012400021211     C                   MOVEL     W0140         TIME              6 0
012500021211     C                   z-add     *zeros        $nrr              5 0
012600021211      *
012700021211      * ovrprtf su file printer
012800131029    1c                   if        wcmd='1'
012900131029     C                   MOVEA     CMD           COMMAN           85
013000131029     c                   else
013100131029     C                   MOVEA     CMD2          COMMAN           85
013200131029    1c                   endif
013300131029     c
013400131029     C                   Z-ADD     85            LUNG             15 5
013500021211      *
013600021211     C                   CALL      'QCMDEXC'                              99
013700021211     C                   PARM                    COMMAN
013800021211     C                   PARM                    LUNG
013900021211      *
014000021211     C                   OPEN      PRTF198
014100131024     c
014200131024     c* lettura del file : elaboro solo le bolle con data < = alla data passata
014300140107     c     *start        setll     fivab46l
014400140107     c                   read      fivab46l
014500131024
014600140107     c                   dow       not %eof(fivab46l)
014700131024     c                   if        vabdsp<=wdataA
014800131024     C                   add       1             $Nrr
014900131024     c                   exsr      stampo
015000140121
015100131024     c                   write     fivab000
015200140121     c* Scrivo il fivax
015300140121    1c                   if        k_key='SED'
015400140121     c     kvab          setll     fivax51l
015500140121     c     kvab          reade     fivax51l
015600140121     c                   dow       not %eof(fivax51l)
015700140121     c                   write     fivax000
015800140121     c                   delete    fivax050
015900140121     c
016000140121     c     kvab          reade     fivax51l
016100140121     c                   enddo
016200140121     c
016300140121     c                   endif
016400131024     c
016500131024     c                   delete    fivab046
016600131024     c                   endif
016700140107     c                   read      fivab46l
016800131024     c                   enddo
016900021211      * Chiudo/Invio
017000140107     C                   CLOSE     FiVab46l
017100131024     c
017200021211      * Stampo spedizioni non trovate
017300131025     c                   if        $nrr=0
017400021211     c   OG
017500021211     cORN77              DO
017600021211     c                   EXCEPT    TESTA
017700021211     c  N77              SETON                                        77
017800021211     c                   ENDDO
017900021211     c                   EXCEPT    DETTA1
018000021211     c                   ENDIF
018100131024
018200021211     c                   SETON                                        LR
018300021211      *
018400021211      * STAMPO DATI DA TRASMETTERE -----------------------------------*
018500021211     C     STAMPO        BEGSR
018600021211      *
018700021211     c   OG
018800021211     cORN77              DO
018900021211     c                   EXCEPT    TESTA
019000021211     c                   EXCEPT    TESTA1
019100021211     c                   EXCEPT    TESTA2
019200021211     c                   EXCEPT    TESTA3
019300021211     c  N77              SETON                                        77
019400021211     c                   ENDDO
019500021211      *
019600021211     c                   movel     vabrsd        Dest             20
019700021211     c                   movel     vabind        Ind              20
019800021211     c                   movel     vabcad        Cap               5
019900021211     c                   movel     vablod        Loc              20
020000021211     c                   EXCEPT    DETTA
020100021211     C                   ENDSR
020200021211      *
020300021211      ********************************************************************
020400021211     oPRTF198   E            TESTA          2 02
020500021211     o                       UDATE             +  2 '  /  /  '
020600021211     o                       TIME              +  2 '  :  :  '
020700021211     o                                         + 35 'Elenco spedizioni inviate '
020800131024     o                                         +  0 'da '
020900131024     o                       winvia
021000131024     o                                         +  0 ' a BOLOGNA Fil'
021100021211     o                                         +  0 'iale'
021200131023     o                                         + 10 'TNSD57R'
021300021211     o                                         +  5 'PAG.'
021400021211     o                       PAGE          Z
021500021211     o          E            TESTA1         2
021600021211     o                                         +  0 '  MM. Nr   Num.    Cd Fil '
021700021211     o                                         +  0 'Destinatario         Indir'
021800021211     o                                         +  0 'izzo            C A P Loca'
021900021211     o                                         +  0 'lit�            Prv E Coll'
022000021211     o                                         +  0 'li       Peso             '
022100021211     o                                         +  0 '   Riferim. Segnac. Segnac'
022200021211     o                                         +  0 '. Zo F Descrizione'
022300021211     o          E            TESTA2         1
022400021211     o                                         +  0 '  GG. Sr   Sped.   Bl Arr '
022500021211     o                                         +  0 '                          '
022600021211     o                                         +  0 '                          '
022700021211     o                                         +  0 '                    s     '
022800021211     o                                         +  0 '         KG               '
022900021211     o                                         +  0 '   Mittente DAL     AL    '
023000021211     o                                         +  0 '  na D  N O T E'
023100021211     o          E            TESTA3         1
023200021211     o                                         +  0 '                   Pa     '
023300021211     o                                         +  0 '                          '
023400021211     o                                         +  0 '                          '
023500021211     o                                         +  0 '                    p     '
023600021211     o                                         + 14 '               Numerico'
023700021211     o          E            DETTA          2
023800021211     o                       VABMGS            +  0 '  /  '
023900021211     o                       VABNRS            +  1
024000021211     o                       VABNSP        2   +  1
024100021211     o                       VABCBO            +  1
024200021211     o                       VABLNA            +  1
024300021211     o                       DEST              +  1
024400021211     o                       IND               +  1
024500021211     o                       CAP               +  1
024600021211     o                       LOC               +  1
024700021211     o                       VABPRD            +  1
024800021211     o                       VABTSP            +  1
024900021211     o                       VABNCL        2   +  1
025000021211     o                       VABPKB        2   +  1
025100021211     o                       VABRMN        2   +  1
025200021211     o                       VABNCD        2   +  2
025300021211     o                       VABNCA        2   +  1
025400021211     o                       VABZNC        2   +  1
025500021211     o                       VABFFD            +  1
025600021211     o                       VABNOT            +  1
025700021211     o          E            DETTA1           06
025800021211     o                                         + 65 '--- NON TROVATE SPE'
025900021211     o                                         +  0 'DIZIONI ---'
026000021211** Comandi    567890123456789012345678901234567890123456789012345678901234567890
026100131025OVRPRTF FILE(PRTF198)  USRDTA('Bolle-ACQ ') HOLD(*YES) SAVE(*YES)
026200131029** Comandi    567890123456789012345678901234567890123456789012345678901234567890
026300131029OVRPRTF FILE(PRTF198)  USRDTA('Bolle-ACQ ') HOLD(*YES) SAVE(*YES) outq(          )
