000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500061017     FTISP20D   CF   E             WORKSTN INFDS(DEVDS1)
000600061017     F                                     SFILE(SP20DMS:MSGULT)
000700061017     Fcap_cap   UF   E             DISK    rename(cap_cap:cap000) prefix(c_)
000800061017     Fcap_loc   UF   E             DISK    rename(cap_loc:loc000) prefix(l_)
000900101101     Flnk_cap   UF   E             DISK    rename(lnk_cap:lnkc00)
001000101115     Flnk_loc   UF   E             DISK    rename(lnk_loc:lnkl00)
001100061017     Fazcpr01l  IF   E           K DISK
001200061017     Fazcpc01l  UF A E           K DISK
001300061017     Fazcpl01l  UF A E           K DISK
001400061024     Fazcpl02l  UF A E           K DISK    rename(azcpl000:azplc002) prefix(l2_)
001500061017     Fazcps01l  UF A E           K DISK
001600061017     Fwfdcpl00f O  A E             DISK    prefix(diff_)
001700061017     FPRTF198   O    F  198        PRINTER
001800061023     FPRTF1982  O    F  198        PRINTER
001900061120     FQSYSPRT   O    F  132        PRINTER
002000000000     D*------------------------------------------------------------------------*
002100000000     D* SCHIERE
002200000000     D*------------------------------------------------------------------------*
002300000000     D*-------------------
002400000000     D* MESSAGGI
002500000000     D*-------------------
002600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700000000     D*-------------------
002800000000     D* DESCRIZIONI TASTI FUNZIONALI
002900000000     D*-------------------
003000000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100000000     D*-------------------
003200000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003300000000     D*-------------------
003400000000     D FUV             S             19    DIM(24)
003500000417     D*-------------------
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900000000     D*-------------------
004000000000     D* RIDEFINIZIONE SCHIERA -FUD-
004100000000     D*-------------------
004200000000     D FUDDS           DS
004300000000     D*                -DESCRIZIONE PARZIALE
004400000000     D  FUDDE1                 1      1
004500000000     D*                -FUNZIONE
004600000000     D  FUDFUN                 2      3
004700000000     D*                -DESCRIZIONE PARZIALE
004800000000     D  FUDDE2                 4     19
004900000000     D*                -FILLER
005000000000     D  FUDFI0                20     34
005100000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005200000000     D  FUDATT                35     35
005300000000     D*                -FILLER
005400000000     D  FUDFI1                36     80
005500000000     D*                -DESCRIZIONE COMPLETA
005600000000     D  FUDDES                 1     19
005700000000     D*                -RIDEFINIZIONE RIGA
005800000000     D  FUDRIG                 1     80
005900000000     D*-------------------
006000000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006100000000     D*-------------------
006200000000     D DSMSG         E DS
006300000000     D*-------------------
006400020917     D* DS REPERIMENTO DATI UTENTE
006500020917     D*------------
006600020917     D TIBS34DS      E DS                                                       *Profili utente
006700020917     D DDATIUTE      E DS                                                       *Dati utente
006800020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900061017     D*-------------------
007000061017     D* ARCHITETTURA
007100061017     D*-------------------
007200061017     D KPJBA         E DS
007300000000     D*-------------------
007400000000     D* DS PER GESTIONE TASTI FUNZIONALI
007500000000     D*-------------------
007600000000     D DSKEY         E DS
007700000000     D DEVDS1          DS
007800000000     D  KEY                  369    369
007900061017     D*------------------------------------------------------------------------*
008000061017     D* DS
008100061017     D*------------------------------------------------------------------------*
008200061017     D*
008300061017     D DSSI00        E DS                  extname(tisp00ds)                    *ds di procedura
008400061017     D*
008500061017     D ds_cpl_obs    E DS                  extname(azcpl00f) prefix(O_)
008600061017     D*
008700061017     D ds_cpc_k      E DS                  extname(azcpc00f) prefix(K_)
008800061017     D ds_cpl_k      E DS                  extname(azcpl00f) prefix(K_)
008900061017     D ds_cps_k      E DS                  extname(azcps00f) prefix(K_)
009000061017     D*
009100061017     D ds_cap_pt     E DS                  extname(cap_cap)  prefix(wc_)
009200061017     D ds_loc_pt     E DS                  extname(cap_loc)  prefix(wl_)
009300061017     D*
009400061017     D ds_cap_sin    E DS                  extname(cap_sin)  prefix(s_)
009500061017     D*
009600061017     D ds_loc_uniq     DS
009700061017     D   uniq_loc                    35
009800061017     D   uniq_prv                     2
009900061023     D   uniq_pra                     2
010000061017     D   uniq_cnt                     9  0
010100061017     D*
010200061017     D ds_loc_rainer   DS
010300061017     D   rainer_loc                  50
010400061017     D   rainer_cap                   9
010500061017     D   rainer_cnt                   9  0
010600061017     D*
010700061017     D ds_com_pt       DS
010800061017     D   com_com                     50
010900061017     D*
011000061017     D ds_loc          DS
011100061017     D   loc_ver                           like(cplver)
011200061017     D   loc_nar                           like(cplnar)
011300061017     D   loc_cap                           like(cplcap)
011400061017     D   loc_loc                           like(cplloc)
011500061024     D*
011600061024     D ds_prv          DS
011700061024     D   prv_prv                           like(cpcprv)
011800061024     D   prv_pra                           like(cpcpra)
011900061023     D*
012000061023     D tisi95ds      e ds
012100061120     D*
012200061120     D ds_cnt_sin      DS                  inz
012300061120     D   cnt_sin_cap                       like(cpscap)
012400061120     D   cnt_sin_sin                       like(cpssin)
012500061017     D*------------------------------------------------------------------------*
012600061017     D* VARIABILI D WRK
012700061017     D*------------------------------------------------------------------------*
012800061017     D wRiga           s            198    inz
012900061023     D wSI95           s            198    inz
013000061017     D wSQL            s            300    inz
013100101112     D wInt1           s            100    inz
013200101112     D wInt2           s            100    inz
013300101112     D wInt3           s            100    inz
013400061017     D wCPCVER         s                   inz like(cpcver)
013500061017     D wCPCCAP         s                   inz like(cpccap)
013600061017     D wCPLLOC         s                   inz like(cplloc)
013700061017     D wCPLESS         s                   inz like(cpless)
013800061023     D wCPLLIV         s                   inz like(cplliv)
013900061017     D wCPSCAP         s                   inz like(cpscap)
014000061017     D wCPSLOC         s                   inz like(cpsloc)
014100061017     D savcap          S                   inz like(cpccap)
014200061017     D chkobs          S              1    inz('1')
014300061122     D key_sin_loc     S                   inz like(cpsloc)
014400061122     D wrk_error       S              1    inz(*blanks)
014500061122     D w_cnt_sin       S              5  0 inz
014600061122     D*------------------------------------------------------------------------*
014700061122     D* DS E SCHIERA X ANALISI PREVENTIVA SITUAZIONE SINONIMI
014800061122     D*------------------------------------------------------------------------*
014900061122     D ds_dup_sin      DS                  inz
015000061122     D   d_dup_fi1                         inz like(cplfi1)
015100061122     D   d_dup_cap                         inz like(cpscap)
015200061122     D   d_dup_sin                         inz like(cpssin)
015300061122     D   d_dup_loc                         inz like(cpsloc)
015400000000     C*------------------------------------------------------------------------*
015500000000     C* MAIN LINE
015600000000     C*------------------------------------------------------------------------*
015700080317     C*
015800080317     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
015900080317     C
016000080317     C/EXEC SQL
016100080317     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016200080317     C/END-EXEC
016300000000     C*
016400000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
016500000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
016600000000     C*
016700000000     C* CICLO FINO A CHE:
016800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
016900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
017000000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
017100000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
017200000000     C     F12L00        OREQ      '1'
017300000000     C     FLFINE        OREQ      '1'
017400000000     C*
017500000000     C* EMETTE FORMATO VIDEO
017600000000     C                   EXSR      BUTFMR
017700000000     C*
017800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
017900000000     C                   EXSR      FUNCON
018000000000     C*
018100000000     C                   END                                                    <-- 2 --
018200000000     C*
018300000000     C                   END                                                    <-- 1 --
018400000000     C*
018500000000     C                   SETON                                        LR
018600000000     C*------------------------------------------------------------------------*
018700000000     C* EMISSIONE FORMATO A VIDEO
018800000000     C*------------------------------------------------------------------------*
018900000000     C     BUTFMR        BEGSR
019000000000     C*
019100000000     C* WRITE RECORD TESTATA
019200061017     C                   WRITE     SP20D01
019300000000     C*
019400000000     C* VISUALIZZAZIONE MESSAGGIO
019500000000     C                   EXSR      SNDMSG
019600000000     C*
019700000000     C* WRITE RECORD FUNZIONI
019800061017     C                   WRITE     SP20D02
019900991019     C*
020000000417     C* EXFMT RECORD DETTAGLIO E DECODIFICA
020100000417     C                   EXSR      DECODI
020200061017     C                   EXFMT     SP20D03
020300991019     C*
020400000000     C* AZZERO FLAG DI ERRORE LIVELLO
020500000000     C                   MOVEL     '0'           ERRL00
020600000000     C*
020700000000     C* AZZERO FLAG DI ERRORE
020800061017     C                   SETOFF                                       40
020900000000     C*
021000000000     C                   ENDSR
021100000000     C*------------------------------------------------------------------------*
021200000000     C* SNDMSG - MANDA MESSAGGIO
021300000000     C*------------------------------------------------------------------------*
021400000000     C     SNDMSG        BEGSR
021500000000     C*
021600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
021700061017     C                   MOVEL     '1'           *IN43
021800061017     C                   WRITE     SP20DMC
021900061017     C                   MOVEL     '0'           *IN43
022000000000     C*
022100000000     C* VISUALIZZAZIONE MESSAGGIO
022200000000     C                   Z-ADD     0             MSGULT            4 0
022300000000     C                   CALL      'XMSG'        XMSGPA
022400000000     C     DSMSER        IFEQ      0
022500000000     C                   MOVEL     DSMSKE        MSGKEY
022600000000     C                   END
022700000000     C                   Z-ADD     1             MSGULT
022800061017     C                   WRITE     SP20DMS                                      SFL MESS
022900061017     C                   WRITE     SP20DMC                                      CONTR MESS
023000000000     C*
023100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
023200000000     C                   MOVEL     *BLANKS       DSMSMS
023300000000     C*
023400000000     C                   ENDSR
023500000000     C*------------------------------------------------------------------------*
023600000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
023700000000     C*------------------------------------------------------------------------*
023800000000     C     ENTGES        BEGSR
023900000000     C*
024000000000     C* CONTROLLA I CAMPI IMMESSI
024100000000     C                   EXSR      DECODI
024200000000     C*
024300000000     C                   ENDSR
024400000000     C*------------------------------------------------------------------------*
024500000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
024600000000     C*------------------------------------------------------------------------*
024700000000     C     F03GES        BEGSR
024800000000     C*
024900000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
025000000000     C                   MOVEL     '1'           F03L00
025100000000     C*
025200000000     C                   ENDSR
025300000000     C*------------------------------------------------------------------------*
025400061017     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
025500000000     C*------------------------------------------------------------------------*
025600991020     C     F06GES        BEGSR
025700991105     C*
025800991105     C                   EXSR      DECODI
025900991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
026000991105     C     ERRL00        IFEQ      '0'
026100101112     C*
026200101112     C* Stampo le intestazioni delle stampe
026300101115     C                   eval      wInt1 = 'SEGNALAZ. CAP + ANOMALIE LOCALITA'
026400101112     C                   except    rigaErr_T
026500101112     C*
026600101115     C                   eval      wInt2 = 'VERIFICA LOCALITA AMBIGUE ' +
026700101115     C                                     '(secondo TISI95)'
026800101112     C                   except    rigaSI95_T
026900101112     C*
027000101115     C                   eval      wInt3 = 'ANNULLAMENTI/ERRORI SINONIMI'
027100101112     C                   except    rigaSin_T
027200991105     C*
027300000414     C* LANCIO LA ROUTINE DI RIEMPIMENTO ARCHIVI CAPPARIO POSTE DA BARTOLINI
027400061017     C                   EXSR      ELABORA
027500061023     C*
027600061023     C* Effettuo lancio TISI95 solo x chiusura
027700061023     C                   CLEAR                   TISI95DS
027800061023     C                   EVAL      I95TLA = 'C'
027900061023     C                   CALL      'TISI95R'
028000061023     C                   PARM                    TISI95DS
028100061023     C*
028200000605     C                   ENDIF
028300000605     C*
028400000000     C                   ENDSR
028500000000     C*------------------------------------------------------------------------*
028600000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
028700000000     C*------------------------------------------------------------------------*
028800000000     C     F12GES        BEGSR
028900000000     C*
029000000000     C* IMPOSTO FLAG DI LIVELLO
029100991103     C                   MOVEL     '1'           F12L00
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
029600000000     C*------------------------------------------------------------------------*
029700000000     C     F24GES        BEGSR
029800000000     C*
029900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
030000000000     C                   EXSR      FUNGES
030100000000     C*
030200000000     C                   ENDSR
030300000000     C*------------------------------------------------------------------------*
030400000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
030500000000     C*------------------------------------------------------------------------*
030600000000     C     HELGES        BEGSR
030700000000     C                   ENDSR
030800991020     C*------------------------------------------------------------------------*
030900061017     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
031000000000     C*------------------------------------------------------------------------*
031100000000     C     DECODI        BEGSR
031200000104     C*
031300000413     C* CONTROLLO VERSIONE CAPPARIO  BARTOLINI
031400000104     C     ERRL00        IFEQ      '0'
031500000104     C                   EXSR      CHKVER
031600000413     C                   ENDIF
031700000104     C*
031800000000     C                   ENDSR
031900000414     C*------------------------------------------------------------------------*
032000061017     C*  CHKVER - CONTROLLA PRESENZA O MENO DELLA VERSIONE BARTOLINI
032100000104     C*------------------------------------------------------------------------*
032200000104     C     CHKVER        BEGSR
032300000413     C*
032400000413     C                   IF        %trim(VIDVER)='?'
032500000413     C*
032600000413     C* lancia il programma di gestione versioni BAR in ricerca
032700000414     C                   CLEAR                   dssi00                         *pulisce ds
032800000414     C                   MOVEL     'R01'         d00op0                         *livello procedura
032900000414     C                   MOVEL     *blanks       d00op1                         *livello programma
033000000414     C                   MOVEL     '0'           d00f03                         *premuto F03
033100000414     C                   MOVEL     '0'           d00f12                         *premuto F12
033200000414     C                   MOVEL     '0'           d00err                         *errore
033300000414     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
033400000414     C                   CALL      'TISI46R'                                    *gestione versioni
033500000414     C                   PARM                    kpjba
033600000414     C                   PARM                    dssi00
033700000413     C* ritorno
033800000414     C                   IF        d00err='1'
033900000413     C                   SETON                                        81
034000000413     C                   MOVEL     MSG(5)        DSMSMS
034100000414     C                   ELSE
034200000414     C                   MOVE(p)   d00ver        VIDVER
034300061017     C                   Z-ADD     d00ver        VIHVER            5 0
034400000414     C                   ENDIF
034500000413     C*
034600000413     C                   ELSE
034700000414     C                   call(e)   'ISNUMERIC'
034800000414     C                   PARM      VIDVER        PiStr            30
034900000414     C                   PARM      ','           PiDecChr          1
035000000414     C                   PARM      *ZEROS        PiVal            30 9
035100000414     C                   PARM      '0'           PiInt             1
035200000414     C                   PARM      '0'           PiNum             1
035300061017     C                   IF        NOT %error AND
035400000414     C                             PiInt=*ON
035500000414     C                   Z-ADD     PiVal         VIHVER
035600061017     C     VIHVER        CHAIN     azcpr01l
035700061017     C                   IF        not %found(azcpr01l)
035800061017     C                   SETON                                        40
035900061017     C                   MOVEL     MSG(8)        DSMSMS
036000061017     C                   MOVEL     '1'           ERRL00
036100061023     C                   ELSE
036200061023     C                   Z-ADD     CPRDDE        datcor            8 0
036300061017     C                   ENDIF
036400000417     C                   ELSE
036500061017     C                   MOVEL     '1'           ERRL00
036600000414     C                   ENDIF
036700000413     C                   ENDIF
036800061017     C*
036900061017     C                   IF        ERRL00 <> '1'
037000061017     C                   Z-ADD     VIHVER        NumVER            5 0
037100061017     C                   Z-ADD     *zeros        NumPrecVER        5 0
037200061017     C                   EVAL      NumPrecVER = NumVER - 1
037300061017     C                   ENDIF
037400000105     C*
037500000104     C                   ENDSR
037600000000     C*------------------------------------------------------------------------*
037700061017     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
037800000000     C*------------------------------------------------------------------------*
037900000000     C     AZZVID        BEGSR
038000061018     C*
038100061018     C                   SETOFF                                       919293
038200061018     C                   SETOFF                                       949596
038300061120     C                   SETOFF                                       9798
038400000000     C*
038500061017     C                   MOVEL     *BLANKS       VIDINZ
038600061017     C                   MOVEL     *BLANKS       VIDIMP
038700061017     C                   MOVEL     *BLANKS       VIDCNOP
038800061017     C                   MOVEL     *BLANKS       VIDLNOP
038900061017     C                   MOVEL     *BLANKS       VIDANNS
039000061120     C                   MOVEL     *BLANKS       VIDANNS2
039100061017     C                   MOVEL     *BLANKS       VIDOBSC
039200061023     C                   MOVEL     *BLANKS       VIDSI95
039300061018     C                   MOVEL     *BLANKS       VID01
039400061018     C                   MOVEL     *BLANKS       VID02
039500061018     C                   MOVEL     *BLANKS       VID03
039600061018     C                   MOVEL     *BLANKS       VID04
039700061018     C                   MOVEL     *BLANKS       VID05
039800061018     C                   MOVEL     *BLANKS       VID06
039900061018     C                   MOVEL     *BLANKS       VID07
040000061120     C                   MOVEL     *BLANKS       VID08
040100061017     C*
040200000104     C                   ENDSR
040300000414     C*------------------------------------------------------------------------*
040400061017     C* ELABORA - ROUTINE DI GENERAZIONE ARCHIVI CAPPARIO POSTE DA BARTOLINI
040500000414     C*------------------------------------------------------------------------*
040600061017     C     ELABORA       BEGSR
040700061018     C*
040800061120     C                   IF        VIDINZ  ='X'
040900061120     C                   EVAL      VID01   = 'Da elaborare'
041000061018     C                   ENDIF
041100061120     C                   IF        VIDIMP  ='X'
041200061120     C                   EVAL      VID02   = 'Da elaborare'
041300061018     C                   ENDIF
041400061120     C                   IF        VIDCNOP ='X'
041500061120     C                   EVAL      VID03   = 'Da elaborare'
041600061018     C                   ENDIF
041700061120     C                   IF        VIDLNOP ='X'
041800061120     C                   EVAL      VID04   = 'Da elaborare'
041900061018     C                   ENDIF
042000061120     C                   IF        VIDSI95 <>*blanks
042100061120     C                   EVAL      VID05   = 'Da elaborare'
042200061023     C                   ENDIF
042300061120     C                   IF        VIDOBSC ='X'
042400061120     C                   EVAL      VID06   = 'Da elaborare'
042500061018     C                   ENDIF
042600061120     C                   IF        VIDANNS ='X'
042700061120     C                   EVAL      VID07   = 'Da elaborare'
042800061023     C                   ENDIF
042900061120     C                   IF        VIDANNS2='X'
043000061120     C                   EVAL      VID08   = 'Da elaborare'
043100061120     C                   ENDIF
043200061018     C                   WRITE     SP20D03
043300000606     C*
043400061017     C* => Inizializza versioe da file d wrk
043500061017     C                   IF        VIDINZ='X'
043600061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
043700061018     C                   EXSR      SNDMSG
043800061017     C                   exsr      inzpt
043900000606     C                   ENDIF
044000000606     C*
044100061017     C* => Importa cappario Poste
044200061017     C                   IF        VIDIMP='X'
044300061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
044400061018     C                   EXSR      SNDMSG
044500061024     C                   exsr      updFCPU
044600061017     C                   exsr      execap                                       Elaborazione cap PT
044700061017     C                   exsr      exeloc                                       Elaborazione loc PT
044800000606     C                   ENDIF
044900000605     C*
045000061017     C* => Obsoleta CAP non in CAP Poste
045100061017     C                   IF        VIDCNOP='X'
045200061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
045300061018     C                   EXSR      SNDMSG
045400061017     C                   EXSR      exever1
045500000605     C                   ENDIF
045600061017     C*
045700061017     C* => Obsoleta LOC non in LOC Poste
045800061017     C                   IF        VIDLNOP='X'
045900061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
046000061018     C                   EXSR      SNDMSG
046100061017     C                   EXSR      exever2
046200061017     C                   ENDIF
046300061023     C*
046400061023     C* => Verifica località "?" tramite TISI95R
046500061023     C                   IF        VIDSI95<>*blanks
046600061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
046700061023     C                   EXSR      SNDMSG
046800061023     C                   EXSR      exever5
046900061023     C                   ENDIF
047000061023     C*
047100061023     C* => Obsoleta CAP con tutte LOC obsolete
047200061023     C                   IF        VIDOBSC='X'
047300061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
047400061023     C                   EXSR      SNDMSG
047500061023     C                   EXSR      exever4
047600061023     C                   ENDIF
047700061023     C*
047800061023     C* => Annulla SIN già LOC (x stesso CAP)
047900061023     C                   IF        VIDANNS='X'
048000061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
048100061023     C                   EXSR      SNDMSG
048200061023     C                   EXSR      exever3
048300061123     C*******
048400061123     C* Lasciare exever6 asteriscato! Usato solo la 1° volta x correggere 1 errore.
048500061123     C* Ora correzzione è già nel giro standard
048600061123     C***                EXSR      exever6    (OK asteriscato - usato solo 1° volta - )
048700061123     C*******
048800061023     C                   ENDIF
048900061120     C*
049000061120     C* => Annulla SIN già SIN (x stesso CAP)
049100061120     C                   IF        VIDANNS2='X'
049200061120     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
049300061120     C                   EXSR      SNDMSG
049400061122     C                   EXSR      exever8
049500061120     C                   ENDIF
049600000414     C*
049700000414     C                   ENDSR
049800020917     C*--------------------------------------------------------------------------------------------*
049900020917     C* REPERISCE I DATI UTENTE
050000020917     C*--------------------------------------------------------------------------------------------*
050100020917     C     REPDATIUTE    BEGSR
050200020917     C*
050300020917     C* INIZIALIZZA VARIABILI DI WRK
050400020917     C                   CLEAR                   TIBS34DS
050500020917     C                   CLEAR                   AZUTEDS
050600020917     C                   CLEAR                   DDATIUTE
050700020917     C*
050800020917     C     *DTAARA       DEFINE    §azute        azuteds
050900020917     C     *DTAARA       DEFINE    §datiute      ddatiute
051000020917     C                   IN(E)     *DTAARA
051100061017     C                   IF        %Error
051200020917     c                   EVAL      I34Tla = 'L'
051300020917     C                   CALL      'TIBS34R'
051400020917     C                   PARM                    Tibs34Ds
051500020917     C                   IN        *DTAARA
051600061017     C                   ENDIF
051700020917     C*
051800020917     C                   ENDSR
051900061017     C*--------------------------------------------------------------------------------------------*
052000061017     C* execap - elaborazione cap
052100061017     C*--------------------------------------------------------------------------------------------*
052200061017     C     execap        BEGSR
052300101101     C*
052400101101     C* ======================================================> nuovo CAP 2010
052500101101     C*
052600101101     C* Scorro tutto il file variazioni cap poste x gestione modifiche cap / inseriimenti / cancellazioni
052700101101     C     *start        setll     lnk_cap
052800101101     C                   read      lnk_cap
052900101101     C                   dow       not %eof(lnk_cap)
053000101101     C*
053100101101     C                   select
053200101101     C*
053300101101     C* Se NUOVO CAP => Listo azione
053400101116     C                   when      OLDC_PRV= *blanks AND OLDC_CAP= *blanks AND
053500101116     C                             NEWC_PRV<>*blanks AND NEWC_CAP<>*blanks
053600101101     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
053700101102     C                             ' => trattsi d CAP completamente NUOVO  '+
053800101101     C                             '( gestire MANUALMENTE )'
053900101101     C                   except    rigaErr
054000101101     C*
054100101101     C* Se ELIM. CAP => Listo azione
054200101116     C                   when      OLDC_PRV<>*blanks AND OLDC_CAP<>*blanks AND
054300101116     C                             NEWC_PRV= *blanks AND NEWC_CAP= *blanks
054400101101     C                   eval      wRiga = OLDC_PRV + ' / ' + OLDC_CAP +
054500101102     C                             ' => presenti elim. totali o parziali  '+
054600101102     C                             '( verificare e gestire MANUALMENTE )'
054700101101     C                   except    rigaErr
054800101101     C*
054900101101     C* Se MODIFICA CAP => gestisco
055000101116     C                   when      OLDC_PRV<>*blanks AND OLDC_CAP<>*blanks AND
055100101116     C                             NEWC_PRV<>*blanks AND NEWC_CAP<>*blanks AND
055200101101     C                             (OLDC_PRV<>NEWC_PRV OR OLDC_CAP<>NEWC_CAP)
055300101101     C*
055400101101     C* Creo il nuovo cap duplicandolo dal suo precedente
055500101101     C                   eval      K_cpcVER = NumVER
055600101101     C                   eval      K_cpcNAR = *blanks
055700101101     C                   eval      K_cpcCAP = OLDC_CAP
055800101101     C     KEYcpc01_C    chain     azcpc01l
055900101101     C                   if        %found(azcpc01l)
056000101101     C*
056100101101     C* Verifico congruenza precisa provincie
056200101101     C                   if        OLDC_PRV <> cpcPRV AND OLDC_PRV <> cpcPRA
056300101101     C*
056400101101     C* Listo eventuale incongruenza
056500101101     C                   eval      wRiga = OLDC_PRV + ' / ' + OLDC_CAP +
056600110511     C                             ' => incongruenza dati BRT: '       +
056700101102     C                             cpcPRV + ' / ' + cpcCAP +
056800101102     C                             '  ( verificare MANUALMENTE )'
056900101101     C                   except    rigaErr
057000101101     C*
057100101101     C* Se tutto coincide => procedo
057200101101     C                   else
057300101101     C*
057400101101     C                   eval      cpcCAP = NEWC_CAP
057500101101     C                   eval      cpcPRV = NEWC_PRV
057600101101     C                   eval      cpcDTR = *all'7'
057700101101     C*
057800101101     C* Quindi inserisco il nuovo CAP
057900101116     C                   write(e)  azcpc000
058000101116     C*
058100101116     C* Listo azione
058200101116     C                   if        %error
058300101116     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
058400101116     C                             ' => fallita duplica da CAP: ' +
058500101116     C                             OLDC_PRV + ' / ' + OLDC_CAP
058600101116     C                   except    rigaErr
058700101116     C                   else
058800101101     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
058900101101     C                             ' => duplicato da CAP: ' +
059000101101     C                             OLDC_PRV + ' / ' + OLDC_CAP
059100101101     C                   except    rigaErr
059200101116     C                   endif
059300101101     C                   endif
059400101101     C                   endif
059500101101     C*
059600101101     C                   endsl
059700101101     C*
059800101101     C* Proseguo con la lettura
059900101101     C                   read      lnk_cap
060000101101     C                   enddo
060100101115     C*
060200101115     C* Scorro tutto il file variazioni loc poste x gestione modifiche loc / inseriimenti / cancellazioni
060300101115     C     *start        setll     lnk_loc
060400101115     C                   read      lnk_loc
060500101115     C                   dow       not %eof(lnk_loc)
060600101115     C*
060700101115     C                   select
060800101115     C*
060900101115     C* Se NUOVA LOC => Listo azione
061000101115     C                   when       OLDL_PRV =  *blanks   AND
061100101115     C                              OLDL_CAP =  *blanks   AND
061200101115     C                              OLDL_LOC =  *blanks   AND
061300101115     C                             (NEWL_PRV <> *blanks   OR
061400101115     C                              NEWL_CAP <> *blanks   OR
061500101115     C                              NEWL_LOC <> *blanks)
061600101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
061700101115     C                             ' => trattsi d LOC completamente NUOVA  '+
061800101115     C                             '( gestire MANUALMENTE )'
061900101115     C                   except    rigaErr
062000101115     C*
062100101115     C* Se ELIM. LOC => Listo azione
062200101115     C                   when      (OLDL_PRV <> *blanks   OR
062300101115     C                              OLDL_CAP <> *blanks   OR
062400101115     C                              OLDL_LOC <> *blanks)  AND
062500101115     C                              NEWL_PRV =  *blanks   AND
062600101115     C                              NEWL_CAP =  *blanks   AND
062700101115     C                              NEWL_LOC =  *blanks
062800101115     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
062900101115     C                             ' => presenti elim. totali o parziali  '+
063000101115     C                             '( verificare e gestire MANUALMENTE )'
063100101115     C                   except    rigaErr
063200101115     C*
063300101115     C* Se MODIFICA LOC => gestisco
063400101115     C                   when      (OLDL_PRV <> *blanks   OR
063500101115     C                              OLDL_CAP <> *blanks   OR
063600101115     C                              OLDL_LOC <> *blanks)  AND
063700101115     C                             (NEWL_PRV <> *blanks   OR
063800101115     C                              NEWL_CAP <> *blanks   OR
063900101115     C                              NEWL_LOC <> *blanks)  AND
064000101115     C                             (NEWL_PRV <> OLDL_PRV  OR
064100101115     C                              NEWL_CAP <> OLDL_CAP  OR
064200101116     C                              NEWL_LOC <> OLDL_LOC) AND
064300101116     C                              OLDL_CAP <> *blanks   AND
064400101116     C                              OLDL_LOC <> *blanks   AND
064500101116     C                              NEWL_CAP <> *blanks   AND
064600101116     C                              NEWL_LOC <> *blanks
064700101115     C*
064800101115     C* Creo la nuova località duplicandola dalla sua precedente
064900101115     C                   eval      K_cplVER = NumVER
065000101115     C                   eval      K_cplNAR = *blanks
065100101115     C                   eval      K_cplCAP = OLDL_CAP
065200101115     C                   eval      K_cplLOC = OLDL_LOC
065300101115     C     KEYcpl01_C    chain     azcpl01l
065400101115     C                   if        %found(azcpl01l)
065500101116     C*
065600101116     C* Innanzitutto obsoletizzo la "vecchia" località
065700101116     C                   movel     'O'           cplFI1
065800101116     C                   movel     *blanks       cplFTR
065900101116     C                   eval      cplDTR = *all'8'
066000101116     C                   update(e) azcpl000
066100101116     C*
066200101116     C* Quindi inserisco la nuova località
066300101116     C                   movel     *blanks       cplFI1
066400101115     C                   eval      cplCAP = NEWL_CAP
066500101115     C                   eval      cplLOC = NEWL_LOC
066600101115     C                   eval      cplDTR = *all'7'
066700101115     C                   write(e)  azcpl000
066800101115     C*
066900101115     C* Listo azione
067000101115     C                   if        %error
067100101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
067200101115     C                                   ' => errore in inserimento nuova LOC'
067300101115     C                   except    rigaErr
067400101115     C                   else
067500101115     C*
067600101115     C* Se tutto ok => porto i sinonimi della "vecchia" località sulla nuova località
067700101115     C                   eval      K_cpsVER = NumVER
067800101115     C                   eval      K_cpsNAR = *blanks
067900101115     C                   eval      K_cpsCAP = OLDL_CAP
068000101115     C                   eval      K_cpsLOC = OLDL_LOC
068100101115     C                   eval      wCPSCAP  = NEWL_CAP
068200101115     C                   eval      wCPSLOC  = NEWL_LOC
068300101115     C***                exsr      movsin
068400101115     C                   exsr      dupsin
068500101115     C*
068600101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
068700101115     C                             ' => duplicata da LOC: ' +
068800101115     C                             OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC
068900101115     C                   except    rigaErr
069000101115     C                   endif
069100101115     C*
069200101115     C* Se vecchia località NON trovata => listo anomalia
069300101115     C                   else
069400101115     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
069500101115     C                                   ' => vecchia LOC non trovata, ' +
069600101115     C                                   'impossibile inserire nuova LOC: '+
069700101115     C                                   NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC
069800101115     C                   except    rigaErr
069900101115     C                   endif
070000101115     C*
070100101115     C                   endsl
070200101115     C*
070300101115     C* Proseguo con la lettura
070400101115     C                   read      lnk_loc
070500101115     C                   enddo
070600101101     C*
070700101101     C* <======================================================
070800061017     C*
070900061017     C* Scorro tutto il file cap poste
071000061017     C     *start        setll     cap_cap
071100061017     C                   read      cap_cap
071200061017     C                   dow       not %eof(cap_cap)
071300061017     C*
071400061017     C                   setoff                                       50
071500061017     C*
071600061017     C* Verifico se trattasi d cap già esistente
071700061017     C                   eval      K_cpcVER = NumVER
071800061017     C                   eval      K_cpcNAR = *blanks
071900061017     C                   eval      K_cpcCAP = c_cap
072000061017     C     KEYcpc01_C    chain     azcpc01l
072100061017     C                   if        %found(azcpc01l)
072200061024     C*
072300061024     C* Se provincia altrenativa già valorizzata e provincia cap PT corrente diversa da entrambe (prv e pra) => listo
072400061024     C                   if        c_prv  <> cpcPRV  AND
072500061024     C                             c_prv  <> cpcPRA  AND
072600061024     C                             c_prv  <> *blanks AND
072700061024     C                             cpcPRV <> *blanks AND
072800061024     C                             cpcPRA <> *blanks
072900101101     C                   eval      wRiga = cpcCAP + '  '    +
073000061024     C                             '=> tentativo scrittura 3 provincie ' +
073100061024     C                             '('+cpcPRV+' '+cpcPRA+' '+c_prv+')'
073200061024     C                   except    rigaErr
073300061024     C                   else
073400061017     C*
073500061017     C* Se cap già esistente verifico se è rimasto sulla stessa provincia
073600061017     C                   if        c_prv = *blanks OR
073700061017     C                             c_prv = cpcPRV  OR
073800061017     C                             c_prv = cpcPRA
073900061017     C                   seton                                        50
074000061017     C                   else
074100061017     C*
074200061017     C* Se provincia altrenativa già valorizzata => listo il cap x successiva verifica manuale
074300061024     C                   if        cpcPRA <> *blanks and cpcFTR <> *blanks
074400061017     C                   eval      wRiga = cpcCAP + '  '    +
074500061017     C                             '=> provincia alternativa sovrascritta ' +
074600061017     C                             '(' + cpcPRA + ')'
074700061017     C                   except    rigaErr
074800061017     C                   endif
074900061017     C*
075000061017     C* Quindi imposto la "vecchia" provincia come provincia alternativa ed imposto come provincia
075100061017     C* quella "nuova" PT
075200061017     C                   eval      cpcPRA = cpcPRV
075300061017     C                   eval      cpcPRV = c_prv
075400061017     C                   eval      cpcFTR = *blanks
075500080326     C                   eval      cpcDTR = *all'8'
075600061017     C                   update(e) azcpc000
075700061017     C                   seton                                        50
075800061017     C                   endif
075900061024     C*
076000061024     C                   endif
076100061017     C*
076200061017     C                   else
076300061017     C*
076400061017     C* Se cap inesistente flaggo apposito campo FLG2 su file cap PT
076500061017     C                   eval      c_FLG2 = 'N'
076600061017     C                   update    cap000
076700061017     C                   eval      wRiga = c_prv + '  '    +
076800061017     C                                     c_cap + '  '    +
076900061017     C                             '=> CAP inesistente!!!'
077000061017     C                   except    rigaErr
077100061017     C                   endif
077200061017     C*
077300061017     C* Se cap PT è stato in qualche modo trattato flaggo record su file PT
077400061017     C                   if        *in50 = *on
077500061017     C                   eval      c_FLG1 = 'S'
077600061017     C                   update(e) cap000
077700061017     C                   endif
077800061017     C*
077900061017     C* Proseguo con la lettura dei cap PT
078000061017     C                   read      cap_cap
078100061017     C                   enddo
078200061017     C*
078300061017     C                   ENDSR
078400061017     C*--------------------------------------------------------------------------------------------*
078500061017     C* exeloc - elaborazione località
078600061017     C*--------------------------------------------------------------------------------------------*
078700061017     C     exeloc        BEGSR
078800061017     C*
078900061017     C* "Normalizzo" le sintassi delle località BART
079000061017     C                   exsr      normloc
079100061017     C*
079200061017     C* Scorro tutto il file località poste
079300061017     C     *start        setll     cap_loc
079400061017     C                   read      cap_loc
079500061017     C                   dow       not %eof(cap_loc)
079600061017     C*
079700061017     C                   setoff                                       51
079800061017     C                   clear                   wfdcpl00
079900061023     C                   clear                   wCPLLIV
080000061017     C*
080100061017     C* 1° => se cap+loc PT trovati esattamente in file località BART (azcpl)
080200061017     C                   eval      K_cplVER = NumVER
080300061017     C                   eval      K_cplNAR = *blanks
080400061017     C                   eval      K_cplCAP = l_cap
080500061017     C                   eval      K_cplLOC = l_loc
080600061017     C     KEYcpl01_C    chain     azcpl01l
080700061017     C                   if        %found(azcpl01l)
080800080404     C                   if        cplFI1 <> *blanks OR
080900080404     C                             cplATB <> *blanks
081000080404     C                   eval      cplFI1 = *blanks
081100080404     C                   eval      cplATB = *blanks
081200080404     C                   eval      cplDTR = *all'6'
081300080404     C                   update    azcpl000
081400080404     C                   endif
081500080404     C*
081600061017     C* Se trovata esattamente => eseguo gestione sinonimi
081700061017     C                   exsr      exesin
081800061017     C                   seton                                        51
081900061023     C                   else
082000061023     C*
082100061023     C* Reperisco la provincia dal cap BART
082200061023     C                   eval      K_cpcVER = NumVER
082300061023     C                   eval      K_cpcNAR = *blanks
082400061023     C                   eval      K_cpcCAP = l_cap
082500061023     C     KEYcpc01_C    chain     azcpc01l
082600061023     C                   if        %found(azcpc01l)
082700061017     C*
082800061017     C* 2° => se cap+loc PT nn trovati esattamente in file località BART (azcpl)
082900061017     C*       verifico se la località è univoca nella provincia
083000061017     C                   exsr      rtvlocuniq
083100061017     C*
083200061023     C* Se località univoca nella provincia procedo con elaborazione particolare
083300061017     C                   if        uniq_cnt = 1
083400061017     C*
083500061017     C* Innanzitutto reperisce i dati della prv+loc corrente
083600061017     C                   exsr      rtvloc
083700061017     C*
083800061023     C* 1° obsoletizzo la località BART
083900061017     C                   eval      K_cplVER = loc_ver
084000061017     C                   eval      K_cplNAR = loc_nar
084100061017     C                   eval      K_cplCAP = loc_cap
084200061017     C                   eval      K_cplLOC = loc_loc
084300061017     C     KEYcpl01_C    chain     azcpl01l
084400061017     C                   if        %found(azcpl01l)
084500061023     C                   eval      wCPLLIV = cplLIV
084600061017     C                   movel     'O'           cplFI1
084700061017     C                   movel     *blanks       cplFTR
084800061017     C                   movel     cplESS        wCPLESS
084900061023     C***                movel     *blanks       cplESS
085000080326     C                   eval      cplDTR = *all'8'
085100061017     C                   update    azcpl000
085200061017     C* Tengo traccia della modifica nel file "diff" - Lato "OLD"
085300061017     C                   eval      wCPCVER = NumPrecVER
085400061017     C                   eval      wCPCCAP = cplCAP
085500061017     C                   exsr      rtvcap
085600061017     C                   eval      diff_cplNAR    = *blanks
085700061017     C                   eval      diff_cplLOC    = cplLOC
085800061017     C                   eval      diff_cpcPRVOLD = cpcPRV
085900061017     C                   eval      diff_cpcPRAOLD = cpcPRA
086000061017     C                   eval      diff_cplCAPOLD = cplCAP
086100061017     C*
086200061017     C                   endif
086300061017     C*
086400061017     C* 2° inserisco nuova loc+cap PT
086500061023     C*
086600061024     C* Se località "oldizzata" era a livello "C" => reperisco i dati d instradamento dal suo cap relativo
086700061024     C                   if        wCPLLIV = 'C'
086800061024     C                   eval      wCPCVER = loc_ver
086900061024     C                   eval      wCPCCAP = loc_cap
087000061024     C                   exsr      cpctocpl
087100061024     C                   endif
087200061023     C*
087300061017     C                   movel     *blanks       cplATB
087400061017     C                   movel     *blanks       cplFI1
087500061017     C                   movel     *blanks       cplFTR
087600061017     C                   movel     wCPLESS       cplESS
087700061023     C* Forzo sempre il nuovo inserimento a livello "L"
087800061023     C                   eval      cplLIV  = 'L'
087900061023     C*
088000061017     C                   eval      cplCAP  = l_cap
088100061017     C                   eval      cplLOC  = l_loc
088200061017     C                   eval      wCPCVER = NumVER
088300061017     C                   eval      wCPCCAP = l_cap
088400061017     C                   exsr      rtvcap
088500061017     C                   if        *in52 = *on
088600080326     C                   eval      cplDTR = *all'9'
088700061017     C                   write(e)  azcpl000
088800061024     C* Sempre se località "obsoletizzata" gestita a livello "C"  aggiorno il flag sul cap della località corrente
088900061024     C* che indica la presenza d eccezioni a livello d località
089000061024     C                   if        wCPLLIV = 'C' and cpcESL <> 'S'
089100061024     C                   eval      cpcESL = 'S'
089200080326     C                   eval      cpcDTR = *all'8'
089300061024     C                   update(e) azcpc000
089400061024     C                   endif
089500061017     C* Tengo traccia della modifica nel file "diff" - Lato "NEW"
089600061017     C                   eval      diff_cplNAR    = *blanks
089700061017     C                   eval      diff_cplLOC    = l_loc
089800061017     C                   eval      diff_cpcPRVNEW = cpcPRV
089900061017     C                   eval      diff_cpcPRANEW = cpcPRA
090000061017     C                   eval      diff_cplCAPNEW = cplCAP
090100061017     C                   write(e)  wfdcpl00
090200061017     C                   endif
090300061017     C*
090400061017     C* 3° porto i sinonimi della "vecchia" località sulla nuova località
090500061017     C                   eval      K_cpsVER = loc_ver
090600061017     C                   eval      K_cpsNAR = loc_nar
090700061017     C                   eval      K_cpsCAP = loc_cap
090800061017     C                   eval      K_cpsLOC = loc_loc
090900061017     C                   eval      wCPSCAP  = l_cap
091000061017     C                   eval      wCPSLOC  = l_loc
091100061023     C***                exsr      movsin
091200061023     C                   exsr      dupsin
091300061017     C*
091400061017     C* 4° inserisco i sinonimi relativi
091500061017     C                   exsr      exesin
091600061017     C*
091700061017     C                   seton                                        51
091800061017     C*
091900061017     C                   else
092000061017     C*
092100061017     C* Se località nn univoca nella provincia la inserisco a livello d cap
092200061017     C                   if        uniq_cnt > 1
092300061017     C                   exsr      insloc
092400061017     C* ...inserisco anche i sinonimi relativi
092500061017     C                   exsr      exesin
092600061017     C*                  eval      wRiga = l_prv + '  '    +
092700061017     C*                                    l_cap + '  '    +
092800061017     C*                                    l_loc + '     ' +
092900061017     C*                            '=> Località nn univoca nella provincia!'
093000061017     C*                  except    rigaErr
093100061017     C                   endif
093200061017     C*
093300061017     C* Se località nn trovata nella provincia la inserisco a livello d cap
093400061017     C                   if        uniq_cnt = 0
093500061017     C                   exsr      insloc
093600061017     C* ...inserisco anche i sinonimi relativi
093700061017     C                   exsr      exesin
093800061017     C*
093900061017     C*                  eval      wRiga = l_prv + '  '    +
094000061017     C*                                    l_cap + '  '    +
094100061017     C*                                    l_loc + '     ' +
094200061017     C*                            '=> Località nn trovata nella provincia!'
094300061017     C*                  except    rigaErr
094400061017     C                   endif
094500061017     C                   endif
094600061024     C*
094700061024     C                   else
094800061024     C                   eval      l_FLG2 = 'N'
094900061024     C                   update    loc000
095000061024     C                   eval      wRiga = l_prv + '  '    +
095100061024     C                                     l_cap + '  '    +
095200061024     C                                     l_loc + '     ' +
095300061024     C                             '=> Località su cap inesistente!'
095400061024     C                   except    rigaErr
095500061024     C                   endif
095600061017     C                   endif
095700061017     C*
095800061017     C* Se località PT è stata in qualche modo trattata flaggo record su file PT
095900061017     C                   if        *in51 = *on
096000061017     C                   eval      l_FLG1 = 'S'
096100061017     C                   update(e) loc000
096200061017     C                   endif
096300061017     C*
096400061017     C* Proseguo con la lettura delle località PT
096500061017     C                   read      cap_loc
096600061017     C                   enddo
096700061018     C*
096800061018     C                   EVAL      VID02 = 'Fatto'
096900061018     C                   SETOFF                                       92
097000061018     C                   WRITE     SP20D03
097100061017     C*
097200061017     C                   ENDSR
097300061017     C*--------------------------------------------------------------------------------------------*
097400061017     C* insloc - inserimento nuova loc BART da loc PT
097500061017     C*--------------------------------------------------------------------------------------------*
097600061017     C     insloc        BEGSR
097700061017     C*
097800061017     C                   clear                   azcpl000
097900061017     C                   movel     *blanks       cplATB
098000061017     C                   movel     *blanks       cplFI1
098100061017     C                   movel     *blanks       cplFTR
098200061017     C                   eval      cplVER = NumVER
098300061017     C                   eval      cplNAR = *blanks
098400061017     C                   eval      cplCAP = l_cap
098500061017     C                   eval      cplLOC = l_loc
098600061017     C                   eval      cplLIV = 'C'
098700061017     C                   eval      cplDLV = 'N'
098800061017     C                   eval      cplESS = 'S'
098900061017     C                   eval      wCPCVER = NumVER
099000061017     C                   eval      wCPCCAP = l_cap
099100061017     C                   exsr      rtvcap
099200061017     C                   if        *in52 = *on
099300080326     C                   eval      cplDTR = *all'9'
099400061017     C                   write(e)  azcpl000
099500061017     C                   else
099600061017     C                   eval      l_FLG2 = 'N'
099700061017     C                   update    loc000
099800061017     C                   eval      wRiga = l_prv + '  '    +
099900061017     C                                     l_cap + '  '    +
100000061017     C                                     l_loc + '     ' +
100100061017     C                             '=> Località su cap inesistente!'
100200061017     C                   except    rigaErr
100300061017     C                   endif
100400061017     C*
100500061017     C                   seton                                        51
100600061017     C*
100700061017     C                   ENDSR
100800061017     C*--------------------------------------------------------------------------------------------*
100900061017     C* exesin - gestione sinonimi
101000061017     C*--------------------------------------------------------------------------------------------*
101100061017     C     exesin        BEGSR
101200061017     C*
101300061017     C*
101400061017     C/EXEC SQL
101500061017     C+ declare C1 cursor for
101600061017     C+ select * from cap_sin
101700061017     C+ where PRV = :l_prv and CAP = :l_cap and LOC = :l_loc
101800061017     C+ order by PRV, CAP, LOC, SIN
101900061017     C+ for update of FLG1
102000061017     C/END-EXEC
102100061017     C
102200061017     C/EXEC SQL
102300061017     C+ open C1
102400061017     C/END-EXEC
102500061017     C
102600061017     C/EXEC SQL
102700061017     C+ Fetch next from C1 into :ds_cap_sin
102800061017     C/END-EXEC
102900061017     C*
103000061017     C* Se reperito qualcosa => analizzo
103100061017     C                   dow       sqlcod = *zeros
103200061017     C                   clear                   azcps000
103300061017     C                   eval      cpsVER = NumVER
103400061017     C                   eval      cpsNAR = *blanks
103500061017     C                   eval      cpsCAP = s_cap
103600061017     C                   eval      cpsLOC = s_loc
103700061017     C                   eval      cpsSIN = s_sin
103800061017     C                   eval      cpsFTR = *blanks
103900080326     C                   eval      cpsDTR = *all'9'
104000061017     C                   write(e)  azcps000
104100061017     C
104200061017     C/EXEC SQL
104300061017     C+     update cap_sin
104400061017     C+     set    FLG1 = 'S'
104500061017     C+     where  CURRENT  OF  C1
104600061017     C/END-EXEC
104700061017     C*
104800061017     C*
104900061017     C/EXEC SQL
105000061017     C+ Fetch next from C1 into :ds_cap_sin
105100061017     C/END-EXEC
105200061017     C*
105300061017     C                   enddo
105400061017     C*
105500061017     C/EXEC SQL
105600061017     C+ close C1
105700061017     C/END-EXEC
105800061017     C*
105900061017     C*
106000061017     C                   ENDSR
106100061017     C*--------------------------------------------------------------------------------------------*
106200061017     C* rtvlocuniq - reperimento univocità località x provincia
106300061017     C*--------------------------------------------------------------------------------------------*
106400061017     C     rtvlocuniq    BEGSR
106500061017     C*
106600061017     C/EXEC SQL
106700061017     C+ declare C2 cursor for
106800061024     C+ select cplloc, cpcprv, cpcpra, count(*)
106900061024     C+ from azcpc00f join azcpl00f on
107000061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
107100061024     C+ where cpcver = :NumVER and cpcnar = ' '
107200061024     C+ and (cpcprv = :cpcPRV or
107300061024     C+ (cpcprv = :cpcPRA and cpcprv<>'  ') or
107400061024     C+ cpcpra = :cpcPRV or
107500061024     C+ (cpcpra = :cpcPRA and cpcpra <> '  '))
107600061024     C+ and cplloc = :l_loc
107700061024     C+ group by cplloc, cpcprv, cpcpra
107800061017     C+ for read only
107900061017     C/END-EXEC
108000061017     C
108100061017     C/EXEC SQL
108200061017     C+ open C2
108300061017     C/END-EXEC
108400061017     C
108500061017     C/EXEC SQL
108600061017     C+ Fetch next from C2 into :ds_loc_uniq
108700061017     C/END-EXEC
108800061017     C*
108900061017     C* Se contatore > 1 => località nn univoca nella provincia
109000061017     C                   if        sqlcod = *zeros
109100061017     C                   else
109200061017     C                   eval      uniq_cnt = 0
109300061017     C                   endif
109400061017     C*
109500061017     C/EXEC SQL
109600061017     C+ close C2
109700061017     C/END-EXEC
109800061017     C*
109900061017     C*
110000061017     C                   ENDSR
110100061017     C*--------------------------------------------------------------------------------------------*
110200061017     C* rtvloc - reperimento località
110300061017     C*--------------------------------------------------------------------------------------------*
110400061017     C     rtvloc        BEGSR
110500061017     C*
110600061017     C/EXEC SQL
110700061017     C+ declare C3 cursor for
110800061017     C+ select cplver, cplnar, cplcap, cplloc
110900061017     C+ from azcpc00f join azcpl00f on
111000061017     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
111100061017     C+ where cpcver = :NumVER and cpcnar = ' '
111200061023     C+ and cpcprv = :uniq_prv and cpcpra = :uniq_pra
111300061023     C+ and cplloc = :l_loc
111400061017     C+ for read only
111500061017     C/END-EXEC
111600061017     C
111700061017     C/EXEC SQL
111800061017     C+ open C3
111900061017     C/END-EXEC
112000061017     C
112100061017     C/EXEC SQL
112200061017     C+ Fetch next from C3 into :ds_loc
112300061017     C/END-EXEC
112400061017     C
112500061017     C/EXEC SQL
112600061017     C+ close C3
112700061017     C/END-EXEC
112800061017     C*
112900061017     C                   ENDSR
113000061024     C*--------------------------------------------------------------------------------------------*
113100061024     C* rtvlocnew - reperimento nuova località (appena inserita da PT)
113200061024     C*--------------------------------------------------------------------------------------------*
113300061024     C     rtvlocnew     BEGSR
113400061024     C*
113500061024     C/EXEC SQL
113600061024     C+ declare C10 cursor for
113700061024     C+ select cplver, cplnar, cplcap, cplloc
113800061024     C+ from azcpc00f join azcpl00f on
113900061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
114000061024     C+ where cpcver = :NumVER and cpcnar = ' '
114100061024     C+ and cpcprv = :prv_prv and cpcpra = :prv_pra
114200061024     C+ and cplloc = :l2_cplLOC and cplfi1 = '  ' and cplftr = ' '
114300061024     C+ for read only
114400061024     C/END-EXEC
114500061024     C
114600061024     C/EXEC SQL
114700061024     C+ open C10
114800061024     C/END-EXEC
114900061024     C
115000061024     C/EXEC SQL
115100061024     C+ Fetch next from C10 into :ds_loc
115200061024     C/END-EXEC
115300061024     C
115400061024     C/EXEC SQL
115500061024     C+ close C10
115600061024     C/END-EXEC
115700061024     C*
115800061024     C                   ENDSR
115900061024     C*--------------------------------------------------------------------------------------------*
116000061024     C* rtvprv - reperimento provincia da cap + loc
116100061024     C*--------------------------------------------------------------------------------------------*
116200061024     C     rtvprv        BEGSR
116300061024     C*
116400061024     C/EXEC SQL
116500061024     C+ declare C9 cursor for
116600061024     C+ select cpcprv, cpcpra
116700061024     C+ from azcpc00f
116800061024     C+ where cpcver = :NumVER and cpcnar = ' '
116900061024     C+ and cpccap = :l2_cplCAP
117000061024     C+ for read only
117100061024     C/END-EXEC
117200061024     C
117300061024     C/EXEC SQL
117400061024     C+ open C9
117500061024     C/END-EXEC
117600061024     C
117700061024     C/EXEC SQL
117800061024     C+ Fetch next from C9 into :ds_prv
117900061024     C/END-EXEC
118000061024     C
118100061024     C/EXEC SQL
118200061024     C+ close C9
118300061024     C/END-EXEC
118400061024     C*
118500061024     C                   ENDSR
118600061017     C*--------------------------------------------------------------------------------------------*
118700061017     C* rtvcap - reperimento cap
118800061017     C*--------------------------------------------------------------------------------------------*
118900061017     C     rtvcap        BEGSR
119000061017     C*
119100061017     C                   setoff                                       52
119200061017     C*
119300061017     C* Verifico se trattasi d cap già esistente
119400061017     C                   eval      K_cpcVER = wCPCVER
119500061017     C                   eval      K_cpcNAR = *blanks
119600061017     C                   eval      K_cpcCAP = wCPCCAP
119700061017     C     KEYcpc01_C    chain     azcpc01l
119800061017     C                   if        %found(azcpc01l)
119900061017     C                   seton                                        52
120000061017     C                   endif
120100061017     C*
120200061017     C                   ENDSR
120300061017     C*--------------------------------------------------------------------------------------------*
120400061017     C* rtvcappt - reperimento cap PT
120500061017     C*--------------------------------------------------------------------------------------------*
120600061017     C     rtvcappt      BEGSR
120700061017     C*
120800061018     C                   setoff                                       54
120900061017     C*
121000061017     C/EXEC SQL
121100061017     C+ declare C4 cursor for
121200061017     C+ select * from cap_cap
121300061018     C+ where cap = :cpcCAP
121400061017     C+ for read only
121500061017     C/END-EXEC
121600061017     C
121700061017     C/EXEC SQL
121800061017     C+ open C4
121900061017     C/END-EXEC
122000061017     C
122100061017     C/EXEC SQL
122200061017     C+ Fetch next from C4 into :ds_cap_pt
122300061017     C/END-EXEC
122400061017     C*
122500061018     C* Se nn trovato => segnalo con indicatore 54
122600061017     C                   if        sqlcod <> *zeros
122700061018     C                   seton                                        54
122800061017     C                   endif
122900061017     C
123000061017     C/EXEC SQL
123100061017     C+ close C4
123200061017     C/END-EXEC
123300061017     C*
123400061017     C                   ENDSR
123500061017     C*--------------------------------------------------------------------------------------------*
123600061017     C* rtvlocpt - reperimento località PT
123700061017     C*--------------------------------------------------------------------------------------------*
123800061017     C     rtvlocpt      BEGSR
123900061017     C*
124000061017     C                   setoff                                       53
124100061017     C*
124200061017     C/EXEC SQL
124300061017     C+ declare C5 cursor for
124400061017     C+ select * from cap_loc
124500061024     C+ where cap = :cplCAP and substr(loc, 1, 35) = :cplLOC
124600061017     C+ for read only
124700061017     C/END-EXEC
124800061017     C
124900061017     C/EXEC SQL
125000061017     C+ open C5
125100061017     C/END-EXEC
125200061017     C
125300061017     C/EXEC SQL
125400061017     C+ Fetch next from C5 into :ds_loc_pt
125500061017     C/END-EXEC
125600061017     C*
125700061017     C* Se nn trovato => segnalo con indicatore 53
125800061017     C                   if        sqlcod <> *zeros
125900061017     C                   seton                                        53
126000061017     C                   endif
126100061017     C
126200061017     C/EXEC SQL
126300061017     C+ close C5
126400061017     C/END-EXEC
126500061017     C*
126600061017     C                   ENDSR
126700061017     C*--------------------------------------------------------------------------------------------*
126800061017     C* rtvcompt - reperimento comune PT
126900061017     C*--------------------------------------------------------------------------------------------*
127000061017     C     rtvcompt      BEGSR
127100061017     C*
127200061017     C                   Z-ADD     *zeros        com_cnt           9 0
127300061017     C*
127400061017     C/EXEC SQL
127500061017     C+ declare C6 cursor for
127600061017     C+ select com from cap_loc
127700061017     C+ where cap = :rainer_cap and loc = com
127800061017     C+ group by com
127900061017     C+ for read only
128000061017     C/END-EXEC
128100061017     C
128200061017     C/EXEC SQL
128300061017     C+ open C6
128400061017     C/END-EXEC
128500061017     C
128600061017     C/EXEC SQL
128700061017     C+ Fetch next from C6 into :ds_com_pt
128800061017     C/END-EXEC
128900061017     C*
129000061017     C* Se reperito qualcosa => analizzo e conto
129100061017     C                   dow       sqlcod = *zeros
129200061017     C                   add       1             com_cnt
129300061017     C*
129400061017     C/EXEC SQL
129500061017     C+ Fetch next from C6 into :ds_com_pt
129600061017     C/END-EXEC
129700061017     C*
129800061017     C                   enddo
129900061017     C*
130000061017     C/EXEC SQL
130100061017     C+ close C6
130200061017     C/END-EXEC
130300061017     C*
130400061017     C                   ENDSR
130500061017     C*--------------------------------------------------------------------------------------------*
130600061017     C* rtvlocrainer - reperimento località x provincia su file "Raineri"
130700061017     C*--------------------------------------------------------------------------------------------*
130800061017     C     rtvlocrainer  BEGSR
130900061017     C*
131000061017     C/EXEC SQL
131100061017     C+ declare C7 cursor for
131200061017     C+ select loc, min(cap), count(*)
131300061017     C+ from cap_rainer
131400061017     C+ where (prv = :cpcPRV or prv = :cpcPRA)
131500061024     C+ and substr(loc, 1, 35) = :cplLOC
131600061017     C+ group by loc
131700061017     C+ for read only
131800061017     C/END-EXEC
131900061017     C
132000061017     C/EXEC SQL
132100061017     C+ open C7
132200061017     C/END-EXEC
132300061017     C
132400061017     C/EXEC SQL
132500061017     C+ Fetch next from C7 into :ds_loc_rainer
132600061017     C/END-EXEC
132700061017     C*
132800061017     C* Se contatore > 1 => località nn univoca nella provincia
132900061017     C                   if        sqlcod = *zeros
133000061017     C                   else
133100061017     C                   eval      rainer_cnt = 0
133200061017     C                   endif
133300061017     C*
133400061017     C/EXEC SQL
133500061017     C+ close C7
133600061017     C/END-EXEC
133700061017     C*
133800061017     C*
133900061017     C                   ENDSR
134000061017     C*--------------------------------------------------------------------------------------------*
134100061017     C* movsin - spostamento sinonimi da "vecchia" località a "nuova" località
134200061017     C*--------------------------------------------------------------------------------------------*
134300061017     C     movsin        BEGSR
134400061017     C*
134500061017     C     KEYcps01_P    setll     azcps01l
134600061017     C                   if        %equal(azcps01l)
134700061017     C     KEYcps01_P    reade     azcps01l
134800061017     C                   dow       not %eof(azcps01l)
134900061017     C                   eval      cpsCAP = wCPSCAP
135000061017     C                   eval      cpsLOC = wCPSLOC
135100061017     C                   eval      cpsFTR = *blanks
135200061023     C* Solo se sinonimo corrente diverso da località sinonimata
135300061023     C                   if        cpsSIN <> cpsLOC
135400080326     C                   eval      cpsDTR = *all'8'
135500061017     C                   update(e) azcps000
135600061023     C                   endif
135700061017     C     KEYcps01_P    reade     azcps01l
135800061017     C                   enddo
135900061017     C                   endif
136000061017     C*
136100061017     C                   ENDSR
136200061017     C*--------------------------------------------------------------------------------------------*
136300061017     C* dupsin - copia sinonimi da "vecchia" località a "nuova" località
136400061017     C*--------------------------------------------------------------------------------------------*
136500061017     C     dupsin        BEGSR
136600061017     C*
136700061017     C     KEYcps01_P    setll     azcps01l
136800061017     C                   if        %equal(azcps01l)
136900061017     C     KEYcps01_P    reade     azcps01l
137000061017     C                   dow       not %eof(azcps01l)
137100061017     C                   eval      cpsCAP = wCPSCAP
137200061017     C                   eval      cpsLOC = wCPSLOC
137300061017     C                   eval      cpsFTR = *blanks
137400061023     C* Solo se sinonimo corrente diverso da località sinonimata
137500061023     C                   if        cpsSIN <> cpsLOC
137600080326     C                   eval      cpsDTR = *all'9'
137700061017     C                   write(e)  azcps000
137800061023     C                   endif
137900061017     C     KEYcps01_P    reade     azcps01l
138000061017     C                   enddo
138100061017     C                   endif
138200061017     C*
138300061017     C                   ENDSR
138400061017     C*--------------------------------------------------------------------------------------------*
138500061017     C* normloc - "normalizzazione" sintassi località BART
138600061017     C*--------------------------------------------------------------------------------------------*
138700061017     C     normloc       BEGSR
138800061017     C*
138900061017     C     NumVER        setll     azcpl01l
139000061017     C                   if        %equal(azcpl01l)
139100061017     C     NumVER        reade     azcpl01l
139200061017     C                   dow       not %eof(azcpl01l)
139300061017     C*
139400061017     C* Verifico presenza caratteri da eliminare
139500061017     C                   setoff                                       45
139600061017     C                   eval      wCPLLOC = cplLOC
139700061017     C                   if        %scan('''':cplLOC) > *zeros
139800061017     C     '''':' '      xlate     cplLOC        cplLOC
139900061017     C                   seton                                        45
140000061017     C                   endif
140100061017     C*
140200061017     C                   if        %scan('"':cplLOC)  > *zeros
140300061017     C     '"':' '       xlate     cplLOC        cplLOC
140400061017     C                   seton                                        45
140500061017     C                   endif
140600061017     C*
140700061017     C                   if        %scan('.':cplLOC)  > *zeros
140800061017     C     '.':' '       xlate     cplLOC        cplLOC
140900061017     C                   seton                                        45
141000061017     C                   endif
141100061017     C*
141200061017     C                   if        %scan('-':cplLOC)  > *zeros
141300061017     C     '-':' '       xlate     cplLOC        cplLOC
141400061017     C                   seton                                        45
141500061017     C                   endif
141600061017     C*
141700061017     C* Se sono state effettuate sostituzioni => gestisco sia località che sinonimi relativi
141800061017     C                   if        *in45 = *on
141900061017     C*
142000061017     C* Compatto gli eventuali spazi ripetuti consecutivamente
142100061017     C                   eval      cplLOC = %trim(cplLOC)
142200061017     C                   z-add     1             wPos              2 0
142300061017     C                   eval      wPos = %scan('  ':cplLOC:wPos)
142400061017     C                   dow       wPos > *zeros AND wPos < %len(%trim(cplLOC))
142500061017     C                   eval      cplLOC = %subst(cplLOC:1:wPos)+
142600061017     C                                      %subst(cplLOC:wPos+2)
142700061017     C                   eval      wPos = %scan('  ':cplLOC:wPos+1)
142800061017     C                   enddo
142900061017     C*
143000061017     C* 1° Sostituisco la località "normalizzata" a quella originale
143100061017     C                   eval      cplFTR = *blanks
143200080326     C                   eval      cplDTR = *all'8'
143300061017     C                   update(e) azcpl000
143400061017     C*
143500061017     C* 2° Inserisco come sinonimo della località "normalizzata" la località originale
143600061017     C                   clear                   azcps000
143700061017     C                   eval      cpsVER = cplVER
143800061017     C                   eval      cpsNAR = cplNAR
143900061017     C                   eval      cpsCAP = cplCAP
144000061017     C                   eval      cpsLOC = cplLOC
144100061017     C                   eval      cpsSIN = wCPLLOC
144200061017     C                   eval      cpsFTR = *blanks
144300080326     C                   eval      cpsDTR = *all'9'
144400061017     C                   write(e)  azcps000
144500061017     C*
144600061017     C* 3° Sposto i sinonimi della località originale su quella "normalizzata"
144700061017     C                   eval      K_cpsVER = cplVER
144800061017     C                   eval      K_cpsNAR = cplNAR
144900061017     C                   eval      K_cpsCAP = cplCAP
145000061017     C                   eval      K_cpsLOC = wCPLLOC
145100061017     C                   eval      wCPSCAP  = cplCAP
145200061017     C                   eval      wCPSLOC  = cplLOC
145300061018     C                   exsr      movsin
145400061017     C*
145500061017     C                   endif
145600061017     C*
145700061017     C* Proseguo con la lettura delle località BART
145800061017     C     NumVER        reade     azcpl01l
145900061017     C                   enddo
146000061017     C                   endif
146100061017     C*
146200061017     C                   ENDSR
146300061017     C*--------------------------------------------------------------------------------------------*
146400061017     C* exever1 - Esecuzione 1° verifica: cap BART no in cap PT => obsoletare cap BART
146500061017     C*--------------------------------------------------------------------------------------------*
146600061017     C     exever1       BEGSR
146700061018     C*
146800061018     C                   EVAL      VID03 = 'In elaborazione'
146900061018     C                   SETON                                        93
147000061018     C                   WRITE     SP20D03
147100061017     C*
147200061017     C* Scorro tutto il file dei cap BART
147300061018     C                   eval      K_cpcVER = NumVER
147400061018     C                   eval      K_cpcNAR = *blanks
147500061017     C     *start        setll     azcpc01l
147600061018     C     KEYcpc01_P    setll     azcpc01l
147700061018     C     KEYcpc01_P    reade     azcpc01l
147800061017     C                   dow       not %eof(azcpc01l)
147900061017     C*
148000061017     C* Se cap nn già obsoleto effettuo le verifiche
148100061018     C                   if        cpcFI1 = *blanks           AND
148200061018     C                             %subst(cpcCAP:4:2) <> '00' AND
148300061018     C                             %subst(cpcCAP:4:2) <> '  '
148400061017     C*
148500061017     C* Verifico se cap BART è in file cap PT
148600061018     C                   exsr      rtvcappt
148700061017     C*
148800061017     C* Se nn trovata flaggo in maniera "evidente" cap corrente BART
148900061018     C                   if        *in54 = *on
149000061018     C                   eval      cpcFI1 = '?'
149100080326     C                   eval      cpcDTR = *all'8'
149200061017     C                   update(e) azcpc000
149300061017     C                   endif
149400061017     C*
149500061017     C                   endif
149600061017     C*
149700061017     C* Proseguo con la lettura dei cap BART
149800061018     C     KEYcpc01_P    reade     azcpc01l
149900061017     C                   enddo
150000061018     C*
150100061018     C                   EVAL      VID03 = 'Fatto'
150200061018     C                   SETOFF                                       93
150300061018     C                   WRITE     SP20D03
150400061017     C*
150500061017     C                   ENDSR
150600061017     C*--------------------------------------------------------------------------------------------*
150700061017     C* exever2 - Esecuzione 2° verifica: loc BART no in loc PT => obsoletare loc BART
150800061017     C*--------------------------------------------------------------------------------------------*
150900061017     C     exever2       BEGSR
151000061018     C*
151100061018     C                   EVAL      VID04 = 'In elaborazione'
151200061018     C                   SETON                                        94
151300061018     C                   WRITE     SP20D03
151400061017     C*
151500061017     C* Scorro tutto il file delle località BART
151600061017     C     *start        setll     azcpl01l
151700061018     C     NumVER        setll     azcpl01l
151800061017     C     NumVER        reade     azcpl01l
151900061017     C                   dow       not %eof(azcpl01l)
152000061017     C*
152100061017     C                   setoff                                       60
152200061017     C                   setoff                                       61
152300061017     C*
152400061017     C* Se impostato flag "esistenza sinonimi" => verifica che sia vero
152500061017     C                   if        cplESS = 'S'
152600061017     C                   eval      cplESS = *blanks
152700061017     C                   seton                                        70
152800061017     C                   eval      K_cpsVER = NumVER
152900061017     C                   eval      K_cpsNAR = *blanks
153000061017     C                   eval      K_cpsCAP = cplCAP
153100061017     C                   eval      K_cpsLOC = cplLOC
153200061017     C     KEYcps01_P    setll     azcps01l
153300061017     C                   if        %equal(azcps01l)
153400061017     C     KEYcps01_P    reade     azcps01l
153500061017     C                   dow       not %eof(azcps01l)
153600061017     C                   if        cpsATB <> 'A'
153700061017     C                   eval      cplESS = 'S'
153800061017     C                   setoff                                       70
153900061017     C                   leave
154000061017     C                   endif
154100061017     C     KEYcps01_P    reade     azcps01l
154200061017     C                   enddo
154300061017     C                   endif
154400061017     C                   endif
154500061017     C*
154600061017     C* Se località nn già obsoleta effettuo le verifiche
154700061018     C                   if        cplFI1 = *blanks           AND
154800061018     C                             %subst(cplCAP:4:2) <> '00' AND
154900061018     C                             %subst(cplCAP:4:2) <> '  '
155000061017     C*
155100061017     C* Verifico se località BART presente in località PT
155200061017     C                   exsr      rtvlocpt
155300061017     C*
155400061017     C* Se nn trovata
155500061017     C                   if        *in53 = *on
155600061017     C                   seton                                        61
155700061017     C*
155800061017     C* Reperisco la provincia dal cap BART
155900061017     C                   eval      K_cpcVER = NumVER
156000061017     C                   eval      K_cpcNAR = *blanks
156100061017     C                   eval      K_cpcCAP = cplCAP
156200061017     C     KEYcpc01_C    chain     azcpc01l
156300061017     C                   if        %found(azcpc01l)
156400061017     C*
156500061017     C* Reperisco il cap PT dal file "Raineri" associato alla loc BART corrente
156600061017     C                   exsr      rtvlocrainer
156700061017     C*
156800061017     C* Se trovata e univoca procedo
156900061017     C                   if        rainer_cnt = 1
157000061017     C*
157100061017     C* Reperisco il comune PT associato al cap della loc BART corrente
157200061017     C                   exsr      rtvcompt
157300061017     C*
157400061017     C* Se trovata e univoco procedo
157500061017     C                   if        com_cnt = 1
157600061017     C*
157700061017     C                   seton                                        60
157800061017     C*
157900061017     C* Gestisco inserendo la località coerrente come sinonimo del comune cui nel file cap PT
158000061017     C* il cap della località BART corrente è associato, x cui =>
158100061017     C*
158200061017     C* 1° Annullo la località corrente
158300061017     C                   eval      cplFI1 = '!'
158400080326     C                   eval      cplDTR = *all'8'
158500061017     C                   update(e) azcpl000
158600061017     C*
158700061017     C* Inserisco come sinonimo del comune la località BART corrente (appena annullata)
158800061017     C                   clear                   azcps000
158900061017     C                   eval      cpsVER = cplVER
159000061017     C                   eval      cpsNAR = cplNAR
159100061017     C                   eval      cpsCAP = rainer_cap
159200061017     C                   eval      cpsLOC = com_com
159300061017     C                   eval      cpsSIN = cplLOC
159400061017     C                   eval      cpsFTR = *blanks
159500080326     C                   eval      cpsDTR = *all'9'
159600061017     C                   write(e)  azcps000
159700061017     C*
159800061017     C* Quindi sposto i sinonimi della località originale sul comune appena reperito
159900061017     C                   eval      K_cpsVER = cplVER
160000061017     C                   eval      K_cpsNAR = cplNAR
160100061017     C                   eval      K_cpsCAP = cplCAP
160200061017     C                   eval      K_cpsLOC = cplLOC
160300061017     C                   eval      wCPSCAP  = rainer_cap
160400061017     C                   eval      wCPSLOC  = com_com
160500061018     C***                exsr      movsin
160600061018     C                   exsr      dupsin
160700061017     C*
160800061017     C                   endif
160900061017     C                   endif
161000061017     C                   endif
161100061017     C*
161200061017     C* Se nn trovata flaggo in maniera "evidente" cap+loc corrente BART
161300061017     C                   if        *in60 = *off
161400061017     C                   eval      cplFI1 = '?'
161500061017     C                   endif
161600061017     C*
161700061017     C                   endif
161800061017     C*
161900061017     C                   endif
162000061017     C*
162100061017     C* Se occorre updatare => eseguo
162200061017     C                   if        (*in60 = *off AND *in61 = *on) OR
162300061017     C                             *in70
162400061017     C                   eval      cplFTR = *blanks
162500080326     C                   eval      cplDTR = *all'8'
162600061017     C                   update(e) azcpl000
162700061017     C                   endif
162800061017     C*
162900061017     C* Proseguo con la lettura delle località BART
163000061017     C     NumVER        reade     azcpl01l
163100061017     C                   enddo
163200061018     C*
163300061018     C                   EVAL      VID04 = 'Fatto'
163400061018     C                   SETOFF                                       94
163500061018     C                   WRITE     SP20D03
163600061017     C*
163700061017     C                   ENDSR
163800061017     C*--------------------------------------------------------------------------------------------*
163900061123     C* exever3 - Esecuzione 3° verifica: eliminare sin BART che sono già loc BART (nn obs) x lo stesso cap
164000061017     C*--------------------------------------------------------------------------------------------*
164100061017     C     exever3       BEGSR
164200061018     C*
164300061023     C                   EVAL      VID07 = 'In elaborazione'
164400061023     C                   SETON                                        97
164500080326     C                   WRITE     SP20D03
164600061017     C*
164700061017     C                   eval      wSQL =
164800061017     C                             'update azcps00f set cpsatb = ''A'', '    +
164900080326     C                             'cpsftr='' '', cpsdtr=88888888 '          +
165000080326     C                             ' where cpsver=' + VIDVER                 +
165100061017     C                             ' and cpscap!!cpssin in (select '         +
165200061017     C                             'cplcap!!cplloc from azcpl00f '           +
165300061017     C                             'where cplver=' + VIDVER + ' and cplFI1 ' +
165400061123     C                             '= '' '')'
165500061017     C*
165600061017     C/EXEC SQL
165700061017     C+ execute immediate: wSQL
165800061017     C/END-EXEC
165900061017     C*
166000061023     C                   EVAL      VID07 = 'Fatto'
166100061023     C                   SETOFF                                       97
166200061018     C                   WRITE     SP20D03
166300061018     C*
166400061017     C                   ENDSR
166500061017     C*------------------------------------------------------------------------*
166600061017     C* exever4 - Esecuzione 4° verifica: obsoleta cap BART aventi tutte relative loc BART obsolete
166700061017     C*------------------------------------------------------------------------*
166800061017     C     exever4       BEGSR
166900061018     C*
167000061018     C                   EVAL      VID06 = 'In elaborazione'
167100061018     C                   SETON                                        96
167200061018     C                   WRITE     SP20D03
167300061017     C*
167400061017     C/EXEC SQL
167500061017     C+ declare C8 cursor for select * from azcpl00f
167600061017     C+ where cplver = :NumVER and cplnar = ' '
167700061017     C+ order by cplcap, cplloc
167800061017     C+ for fetch only
167900061017     C/END-EXEC
168000061017     C
168100061017     C/EXEC SQL
168200061017     C+ open C8
168300061017     C/END-EXEC
168400061017     C
168500061017     C/EXEC SQL
168600061017     C+ Fetch C8 into :ds_cpl_obs
168700061017     C/END-EXEC
168800061017     C
168900061017     C                   dow       sqlcod=*zeros
169000061017     C*
169100061017     C* ESEGUO CONSIDERAZIONI ED OPERAZIONI DI SCRITTURA ENTRO CICLO
169200061017     C                   if        O_cplcap = savcap
169300061017     C                   if        O_cplfi1 = *blanks
169400061017     C                   movel     '0'           chkobs
169500061017     C                   endif
169600061017     C                   else
169700061017     C                   if        chkobs = '1' and savcap <> *blanks
169800061017     C                   eval      K_cpcVER = NumVER
169900061017     C                   eval      K_cpcNAR = *blanks
170000061017     C                   eval      K_cpcCAP = savcap
170100061017     C     KEYcpc01_C    chain     azcpc01l
170200061017     C                   if        %found(azcpc01l) AND cpcFI1 <> 'O'
170300061017     C                   movel     'O '          cpcFI1
170400061017     C                   movel     *blanks       cpcFTR
170500080326     C                   eval      cpcDTR = *all'8'
170600061017     C                   update(e) azcpc000
170700061017     C                   endif
170800061017     C                   endif
170900061017     C                   movel     O_cplcap      savcap
171000061017     C                   movel     '1'           chkobs
171100061017     C                   if        O_cplfi1 = *blanks
171200061017     C                   movel     '0'           chkobs
171300061017     C                   endif
171400061017     C                   endif
171500061017     C
171600061017     C/EXEC SQL
171700061017     C+ Fetch C8 into :ds_cpl_obs
171800061017     C/END-EXEC
171900061017     C
172000061017     C                   enddo
172100061017     C
172200061017     C/EXEC SQL
172300061017     C+ close C8
172400061017     C/END-EXEC
172500061018     C*
172600061018     C                   EVAL      VID06 = 'Fatto'
172700061018     C                   SETOFF                                       96
172800061018     C                   WRITE     SP20D03
172900061017     C*
173000061017     C                   ENDSR
173100061023     C*--------------------------------------------------------------------------------------------*
173200061023     C* exever5 - Esecuzione 5° verifica: vaglia loc BART "?" tramite TISI95R
173300061023     C*--------------------------------------------------------------------------------------------*
173400061023     C     exever5       BEGSR
173500061023     C*
173600061023     C                   EVAL      VID05 = 'In elaborazione'
173700061023     C                   SETON                                        95
173800061023     C                   WRITE     SP20D03
173900061023     C*
174000061023     C* Scorro tutto il file delle località BART
174100061023     C     *start        setll     azcpl01l
174200061023     C     NumVER        setll     azcpl01l
174300061023     C     NumVER        reade     azcpl01l
174400061023     C                   dow       not %eof(azcpl01l)
174500061024     C*
174600061024     C                   setoff                                       58
174700061023     C*
174800061023     C* Se località già battezzata "?" elaboro
174900061023     C                   if        %trim(cplFI1) = '?'
175000061023     C*
175100061023     C* Reperisco la provincia dal cap BART
175200061023     C                   eval      K_cpcVER = NumVER
175300061023     C                   eval      K_cpcNAR = *blanks
175400061023     C                   eval      K_cpcCAP = cplCAP
175500061023     C     KEYcpc01_C    chain     azcpc01l
175600061023     C                   if        %found(azcpc01l)
175700061023     C*
175800061023     C* Passo al TISI95R loc + prv e se livello affidabilità >= 2 listo e tengo traccia aggiornando la località
175900061023     C                   clear                   TISI95DS
176000061023     C                   eval      I95TCN = '7'
176100061023     C                   z-add     datcor        I95DAT
176200061023     C                   eval      I95LOC = cplLOC
176300061023     C                   eval      I95PRV = cpcPRV
176400061023     C                   call      'TISI95R'
176500061023     C                   parm                    TISI95DS
176600061023     C*
176700061023     C                   if        O95LIA >= '2'
176800061024     C*
176900061024     C                   seton                                        58
177000061023     C*
177100061023     C* 1° Listo in stampa x preventiva verifica manuale
177200061023     C                   eval      wSI95 = 'DA: ' +
177300061023     C                                     cplCAP + ' ' +
177400061023     C                                     cplLOC + ' ' +
177500061023     C                                     cpcPRV + ' ' +
177600061023     C                                     ' => A: ' +
177700061023     C                                     O95CAP + ' ' +
177800061023     C                                     O95LOC + ' ' +
177900061023     C                                     O95PRV + ' ' +
178000061023     C                                     O95SIN + ' ' +
178100061023     C                                     O95LID + ' ' +
178200061023     C                                     O95LIA
178300061023     C                   except    rigaSI95
178400061023     C*
178500061023     C* 2° Marco la località corrente (solo se lancio no in simulazione)
178600061023     C                   if        VIDSI95 = 'X'
178700061023     C                   eval      cplFI1  = 'T'
178800080326     C                   eval      cplDTR = *all'8'
178900061023     C                   update(e) azcpl000
179000061023     C*
179100061023     C* 3° Sposto i sinonimi della località originale sul comune appena reperito
179200061023     C                   eval      K_cpsVER = cplVER
179300061023     C                   eval      K_cpsNAR = cplNAR
179400061023     C                   eval      K_cpsCAP = cplCAP
179500061023     C                   eval      K_cpsLOC = cplLOC
179600061204     C                   eval      wCPSCAP  = O95CAP
179700061204     C                   eval      wCPSLOC  = O95LOC
179800061023     C***                exsr      movsin
179900061024     C                   exsr      dupsin
180000061023     C                   endif
180100061023     C*
180200061023     C                   endif
180300061023     C                   endif
180400061023     C                   endif
180500061024     C*
180600061024     C* Se x qualunque ragione nn ho elaborato => listo
180700061024     C***                if        *in58 = *off
180800061024     C***                eval      wSI95 = 'DA: ' +
180900061024     C***                                  cplCAP + ' ' +
181000061024     C***                                  cplLOC + ' ' +
181100061024     C***                                  cpcPRV + ' ' +
181200061024     C***                                  ' => N.P. '
181300061024     C***                except    rigaSI95
181400061024     C***                endif
181500061023     C*
181600061023     C* Proseguo con la lettura delle località BART
181700061023     C     NumVER        reade     azcpl01l
181800061023     C                   enddo
181900061023     C*
182000061023     C                   EVAL      VID05 = 'Fatto'
182100061023     C                   SETOFF                                       95
182200061023     C                   WRITE     SP20D03
182300061023     C*
182400061023     C                   ENDSR
182500061024     C*--------------------------------------------------------------------------------------------*
182600061024     C* exever6 - Esecuzione 6° verifica: sistema le LOC BART obsolete e a livello "C" x
182700061024     C*                                   ricongiunzione a nuova località su diverso cap  ma in
182800061024     C*                                   stessa provincia / provincia alternativa
182900061024     C*--------------------------------------------------------------------------------------------*
183000061024     C     exever6       BEGSR
183100061024     C*
183200061024     C                   EVAL      VID07 = 'In elaborazione'
183300061024     C                   SETON                                        97
183400061024     C                   WRITE     SP20D03
183500061024     C*
183600061024     C* Scorro tutto il file delle località BART
183700061024     C     *start        setll     azcpl02l
183800061024     C     NumVER        setll     azcpl02l
183900061024     C     NumVER        reade     azcpl02l
184000061024     C                   dow       not %eof(azcpl02l)
184100061024     C*
184200061024     C* Elaboro località obsolete e a livello "C"
184300061024     C                   if        l2_cplFI1 = 'O ' AND
184400061024     C                             l2_cplLIV = 'C'
184500061024     C*
184600061024     C* Reperisco innanzitutto la provincia / prov alternativa della località corrente
184700061024     C                   clear                   ds_prv
184800061024     C                   exsr      rtvprv
184900061024     C*
185000061024     C* Se reperito qualcosa proseguo con elaborazione
185100061024     C                   if        ds_prv <> *blanks
185200061024     C*
185300061024     C* Quindi reperisco la località precisa
185400061024     C                   clear                   ds_loc
185500061024     C                   exsr      rtvlocnew
185600061024     C*
185700061024     C* Se reperito qualcosa proseguo con elaborazione
185800061024     C                   if        ds_loc <> *blanks
185900061024     C*
186000061024     C* Quindi aggancio la nuova località inserita da PT
186100061024     C                   eval      K_cplVER = loc_ver
186200061024     C                   eval      K_cplNAR = loc_nar
186300061024     C                   eval      K_cplCAP = loc_cap
186400061024     C                   eval      K_cplLOC = loc_loc
186500061024     C     KEYcpl01_C    chain     azcpl01l
186600061024     C                   if        %found(azcpl01l)
186700061024     C*
186800061024     C* Reperisco i dati d instradamento dal cap della relativa località "obsoletizzata"
186900061024     C                   eval      wCPCVER = l2_cplVER
187000061024     C                   eval      wCPCCAP = l2_cplCAP
187100061024     C                   exsr      cpctocpl
187200061024     C*
187300061024     C* Imposto quindi a "L" il livello d gestioine della nuova località inserita da PT
187400061024     C                   eval      cplLIV  = 'L'
187500061024     C*
187600061024     C* Superfluo ma cmq sfleggo il record
187700061024     C                   movel     *blanks       cplFTR
187800061024     C*
187900061024     C* Quindi aggiorno il record della nuova località inserita da PT
188000080326     C                   eval      cplDTR = *all'8'
188100061024     C                   update(e) azcpl000
188200061024     C*
188300061024     C* A questo punto occorre indicare sul cap della nuova localita inserita da PT la presenza d eccezioni x localita
188400061024     C                   eval      wCPCVER = loc_ver
188500061024     C                   eval      wCPCCAP = loc_cap
188600061024     C                   exsr      rtvcap
188700061024     C                   if        *in52 = *on
188800061024     C                   eval      cpcESL = 'S'
188900080326     C                   eval      cpcDTR = *all'8'
189000061024     C                   update(e) azcpc000
189100061024     C                   endif
189200061024     C                   endif
189300061024     C                   endif
189400061024     C                   endif
189500061024     C                   endif
189600061024     C*
189700061024     C* Proseguo con la lettura delle località BART
189800061024     C     NumVER        reade     azcpl02l
189900061024     C                   enddo
190000061024     C*
190100061024     C                   EVAL      VID07 = 'Fatto'
190200061024     C                   SETOFF                                       97
190300061024     C                   WRITE     SP20D03
190400061024     C*
190500061122     C                   ENDSR
190600061122     C*--------------------------------------------------------------------------------------------*
190700061122     C* exever8 - Esecuzione 8° verifica: annullaamento sinonimi doppi su stesso cap
190800061122     C*--------------------------------------------------------------------------------------------*
190900061122     C     exever8       BEGSR
191000061122     C*
191100061122     C                   EVAL      VID08 = 'In elaborazione'
191200061122     C                   SETON                                        98
191300061122     C                   WRITE     SP20D03
191400061122     C*
191500061122     C* Inizializzo il contatore d stesso sin su stesso cap
191600061122     C                   eval      w_cnt_sin = *zeros
191700061122     C*
191800061122     C/EXEC SQL
191900061122     C+ declare C11 cursor for
192000061122     C+ select cplfi1, cpscap, cpssin, cpsloc from azcps00f join azcpl00f
192100061122     C+ on  cpsver=cplver and cpsnar=cplnar and cpscap=cplcap
192200061122     C+ and cpsloc=cplloc
192300061123     C+ where cpsver=:NumVER
192400061122     C+ order by cpscap, cpssin, cplfi1
192500061122     C+ for read only
192600061122     C/END-EXEC
192700061122     C
192800061122     C/EXEC SQL
192900061122     C+ open C11
193000061122     C/END-EXEC
193100061122     C
193200061122     C/EXEC SQL
193300061122     C+ Fetch next from C11 into :ds_dup_sin
193400061122     C/END-EXEC
193500061122     C*
193600061122     C* Se reperito qualcosa => analizzo
193700061122     C                   dow       sqlcod = *zeros
193800061122     C*
193900061122     C* Verifico se già presente x tesso cap
194000061122     C                   if        d_dup_cap = cnt_sin_cap AND
194100061122     C                             d_dup_sin = cnt_sin_sin
194200061122     C*
194300061122     C* Incremento il contatore d stesso sin su stesso cap
194400061122     C                   eval      w_cnt_sin = w_cnt_sin + 1
194500061122     C*
194600061122     C* Se contatore > 1 => annullo il sin corrente
194700061122     C                   if        w_cnt_sin > 1
194800061122     C                   eval      key_sin_loc = d_dup_loc
194900061122     C                   exsr      annSIN
195000061122     C                   endif
195100061122     C*
195200061122     C* Se nn già presente salvo cap/sin correnti
195300061122     C                   else
195400061122     C*
195500061122     C* Re-imposto a 1 il contatore d sin x stesso cap
195600061122     C                   eval      w_cnt_sin = 1
195700061122     C*
195800061122     C* Quindi salvo il cap/sin corrente
195900061122     C                   eval      cnt_sin_cap = d_dup_cap
196000061122     C                   eval      cnt_sin_sin = d_dup_sin
196100061122     C                   endif
196200061122     C*
196300061122     C* Proseguo con la lettura dei sin
196400061122     C*
196500061122     C/EXEC SQL
196600061122     C+ Fetch next from C11 into :ds_dup_sin
196700061122     C/END-EXEC
196800061122     C*
196900061122     C                   enddo
197000061122     C*
197100061122     C/EXEC SQL
197200061122     C+ close C11
197300061122     C/END-EXEC
197400061122     C*
197500061122     C                   EVAL      VID08 = 'Fatto'
197600061122     C                   SETOFF                                       98
197700061122     C                   WRITE     SP20D03
197800061122     C*
197900061122     C                   ENDSR
198000061120     C*--------------------------------------------------------------------------------------------*
198100061120     C* annSIN - annulla sinonimo
198200061120     C*--------------------------------------------------------------------------------------------*
198300061120     C     annSIN        BEGSR
198400061120     C*
198500061120     C                   eval      K_cpsVER = NumVER
198600061120     C                   eval      K_cpsNAR = *blanks
198700061122     C                   eval      K_cpsCAP = d_dup_cap
198800061120     C                   eval      K_cpsLOC = key_sin_loc
198900061122     C                   eval      K_cpsSIN = d_dup_sin
199000061120     C     KEYcps01_C    chain(e)  azcps01l
199100061120     C                   if        %error
199200061120     C                   eval      wrk_error = 'E'
199300061120     C                   except    rigaSin
199400061120     C                   else
199500061120     C                   if        %found(azcps01l)
199600061120     C                   eval      cpsATB = 'A'
199700061120     C                   eval      cpsFTR = *blanks
199800080326     C                   eval      cpsDTR = *all'8'
199900061120     C                   update    azcps000
200000061120     C                   eval      wrk_error = *blanks
200100061120     C                   except    rigaSin
200200061120     C                   endif
200300061120     C                   endif
200400061120     C*
200500061120     C                   ENDSR
200600061024     C*--------------------------------------------------------------------------------------------*
200700061024     C* updFCPU - aggiornamento provincie alternative
200800061024     C*--------------------------------------------------------------------------------------------*
200900061024     C     updFCPU       BEGSR
201000061024     C*
201100061024     C                   EVAL      VID02 = 'In elaborazione'
201200061024     C                   SETON                                        92
201300061024     C                   WRITE     SP20D03
201400061024     C*
201500061024     C                   eval      wSQL = 'update azcpc00f ' +
201600061024     C                             'set cpcPRV=''FC'', cpcPRA=''FO'' ' +
201700061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''FO'' '+
201800061024     C                             'and cpcPRA=''FC'''
201900061024     C*
202000061024     C/EXEC SQL
202100061024     C+ execute immediate: wSQL
202200061024     C/END-EXEC
202300061024     C*
202400061024     C                   eval      wSQL = 'update azcpc00f ' +
202500061024     C                             'set cpcPRV=''PU'', cpcPRA=''PS'' ' +
202600061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''PS'' '+
202700061024     C                             'and cpcPRA=''PU'''
202800061024     C*
202900061024     C/EXEC SQL
203000061024     C+ execute immediate: wSQL
203100061024     C/END-EXEC
203200061024     C*
203300061024     C                   ENDSR
203400061017     C*--------------------------------------------------------------------------------------------*
203500061017     C* inzpt - inizializzazione flag di elaborazione file PT
203600061017     C*--------------------------------------------------------------------------------------------*
203700061017     C     inzpt         BEGSR
203800061018     C*
203900061018     C                   EVAL      VID01 = 'In elaborazione'
204000061018     C                   SETON                                        91
204100061018     C                   WRITE     SP20D03
204200061017     C*
204300061017     C                   eval      wSQL = 'update cap_cap set FLG1 = '' '', ' +
204400061017     C                                                       'FLG2 = '' '''
204500061017     C*
204600061017     C/EXEC SQL
204700061017     C+ execute immediate: wSQL
204800061017     C/END-EXEC
204900061017     C*
205000061017     C                   eval      wSQL = 'update cap_loc set FLG1 = '' '', ' +
205100061017     C                                                       'FLG2 = '' '''
205200061017     C*
205300061017     C/EXEC SQL
205400061017     C+ execute immediate: wSQL
205500061017     C/END-EXEC
205600061017     C                   eval      wSQL = 'update cap_sin set FLG1 = '' '', ' +
205700061017     C                                                       'FLG2 = '' '''
205800061017     C*
205900061017     C/EXEC SQL
206000061017     C+ execute immediate: wSQL
206100061017     C/END-EXEC
206200061017     C*
206300061017     C                   eval      wSQL = 'delete from azcpc00f where cpcver='+
206400061017     C                                    VIDVER
206500061017     C*
206600061017     C/EXEC SQL
206700061017     C+ execute immediate: wSQL
206800061017     C/END-EXEC
206900061017     C*
207000061017     C                   eval      wSQL = 'delete from azcpl00f where cplver='+
207100061017     C                                    VIDVER
207200061017     C*
207300061017     C/EXEC SQL
207400061017     C+ execute immediate: wSQL
207500061017     C/END-EXEC
207600061017     C                   eval      wSQL = 'delete from azcps00f where cpsver='+
207700061017     C                                    VIDVER
207800061017     C*
207900061017     C/EXEC SQL
208000061017     C+ execute immediate: wSQL
208100061017     C/END-EXEC
208200061017     C*
208300061017     C                   CALL      'TISP20C1'
208400061017     C*
208500061017     C                   eval      wSQL = 'update azcpc00f set cpcFTR = ''T'' '+
208600061017     C                                    'where cpcver='+VIDVER
208700061017     C*
208800061017     C/EXEC SQL
208900061017     C+ execute immediate: wSQL
209000061017     C/END-EXEC
209100061017     C*
209200061017     C                   eval      wSQL = 'update azcpl00f set cplFTR = ''T'' '+
209300061017     C                                    'where cplver='+VIDVER
209400061017     C*
209500061017     C/EXEC SQL
209600061017     C+ execute immediate: wSQL
209700061017     C/END-EXEC
209800061017     C                   eval      wSQL = 'update azcps00f set cpsFTR = ''T'' '+
209900061017     C                                    'where cpsver='+VIDVER
210000061017     C*
210100061017     C/EXEC SQL
210200061017     C+ execute immediate: wSQL
210300061017     C/END-EXEC
210400061018     C*
210500061018     C                   EVAL      VID01 = 'Fatto'
210600061018     C                   SETOFF                                       91
210700061018     C                   WRITE     SP20D03
210800061017     C*
210900061017     C                   ENDSR
211000061024     C*--------------------------------------------------------------------------------------------*
211100061024     C* cpctocpl - passaggio dati da livello "C" a "L"
211200061024     C*--------------------------------------------------------------------------------------------*
211300061024     C     cpctocpl      BEGSR
211400061024     C*
211500061024     C                   exsr      rtvcap
211600061024     C                   if        *in52 = *on
211700061024     C                   eval      cplLNA = cpcLNA
211800061024     C                   eval      cplZNC = cpcZNC
211900061024     C                   eval      cplISO = cpcISO
212000061024     C                   eval      cplZFD = cpcZFD
212100061024     C                   eval      cplLKG = cpcLKG
212200061024     C                   eval      cplLMC = cpcLMC
212300061024     C                   eval      cplLOL = cpcLOL
212400061024     C                   eval      cplZOL = cpcZOL
212500061024     C                   eval      cplLKS = cpcLKS
212600061024     C                   eval      cplLMS = cpcLMS
212700061024     C                   eval      cplLOS = cpcLOS
212800061024     C                   eval      cplZOS = cpcZOS
212900061024     C                   eval      cplLK2 = cpcLK2
213000061024     C                   eval      cplLM2 = cpcLM2
213100061024     C                   eval      cplLO2 = cpcLO2
213200061024     C                   eval      cplZO2 = cpcZO2
213300061024     C                   eval      cplCTS = cpcCTS
213400061024     C                   eval      cplCTE = cpcCTE
213500061024     C                   eval      cplTTC = cpcTTC
213600061024     C                   eval      cplPII = cpcPII
213700061024     C                   eval      cplPID = cpcPID
213800061024     C                   eval      cplPIS = cpcPIS
213900061024     C                   eval      cplGDD = cpcGDD
214000061121     C                   eval      cplFI2 = cpcFI2
214100061024     C                   endif
214200061024     C*
214300061024     C                   ENDSR
214400000000     C*------------------------------------------------------------------------*
214500980403     C* *INZSR - OPERAZIONI INIZIALI
214600000000     C*------------------------------------------------------------------------*
214700980403     C     *INZSR        BEGSR
214800000000     C*
214900000000     C* RICEVIMENTO PARAMETRI
215000000000     C     *ENTRY        PLIST
215100000000     C                   PARM                    KPJBA
215200061017     C*
215300061017     C* Definizione chiavi di lettura
215400061017     C*
215500061017     C* AZCPC01L - Completa
215600061017     C     KEYcpc01_C    KLIST
215700061017     C                   KFLD                    K_cpcVER
215800061017     C                   KFLD                    K_cpcNAR
215900061017     C                   KFLD                    K_cpcCAP
216000061018     C*
216100061018     C* AZCPC01L - Parziale
216200061018     C     KEYcpc01_P    KLIST
216300061018     C                   KFLD                    K_cpcVER
216400061018     C                   KFLD                    K_cpcNAR
216500061017     C*
216600061017     C* AZCPL01L - Completa
216700061017     C     KEYcpl01_C    KLIST
216800061017     C                   KFLD                    K_cplVER
216900061017     C                   KFLD                    K_cplNAR
217000061017     C                   KFLD                    K_cplCAP
217100061017     C                   KFLD                    K_cplLOC
217200061017     C*
217300061017     C* AZCPS01L - Parziale
217400061017     C     KEYcps01_P    KLIST
217500061017     C                   KFLD                    K_cpsVER
217600061017     C                   KFLD                    K_cpsNAR
217700061017     C                   KFLD                    K_cpsCAP
217800061017     C                   KFLD                    K_cpsLOC
217900061120     C*
218000061120     C* AZCPS01L - Completa
218100061120     C     KEYcps01_C    KLIST
218200061120     C                   KFLD                    K_cpsVER
218300061120     C                   KFLD                    K_cpsNAR
218400061120     C                   KFLD                    K_cpsCAP
218500061120     C                   KFLD                    K_cpsLOC
218600061120     C                   KFLD                    K_cpsSIN
218700000104     C*
218800000104     C* AZZERO INIZIALE INDICATORI
218900061017     C                   SETOFF                                       40
219000000000     C*
219100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
219200000000     C     XMSGPA        PLIST
219300000000     C                   PARM                    DSMSMS
219400000000     C                   PARM                    DSMSPG
219500000000     C                   PARM                    DSMSKE
219600000000     C                   PARM                    DSMSER
219700000000     C*
219800000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
219900000000     C                   CALL      'XKEY'
220000000000     C                   PARM                    DSKEY
220100000000     C*
220200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
220300000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
220400000000     C*
220500000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
220600000000     C*   -NOME PGM CHIAMANTE
220700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
220800061017     C                   MOVEL     'TISP20R'     DSMSPG
220900061017     C                   MOVEL     'TISP20R'     MSGQUE
221000000000     C                   MOVEL     *BLANKS       MSGKEY
221100000000     C                   MOVEL     *BLANKS       DSMSMS
221200000000     C*
221300000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
221400000000     C                   Z-ADD     0             FUNGRU            5 0
221500000000     C*
221600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
221700000000     C                   EXSR      FUNGES
221800000000     C*
221900000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
222000000000     C                   MOVEL     '0'           F12L00            1
222100000000     C                   MOVEL     '0'           F03L00            1
222200000000     C                   MOVEL     '0'           FINL00            1
222300000000     C                   MOVEL     '0'           ERRL00            1
222400000000     C                   MOVEL     '0'           FLFINE            1
222500000000     C*
222600000000     C* LANCIO ROUTINE PER VERIFICARE SE
222700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
222800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
222900020917     C                   EXSR      REPDATIUTE
223000000000     C*
223100000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
223200000000     C                   EXSR      AZZVID
223300000000     C*
223400000000     C                   ENDSR
223500000000     C*------------------------------------------------------------------------*
223600000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
223700000000     C*------------------------------------------------------------------------*
223800000000     C     FUNGES        BEGSR
223900000000     C*
224000000000     C* PULISCO CAMPI DI LAVORO
224100000000     C                   Z-ADD     0             W                 5 0
224200000000     C*
224300000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
224400000000     C* E CARICO TABELLA FUNZIONI ATTIVE
224500000000     C     1             DO        24            I
224600000000     C                   MOVEL     FUD(I)        FUDRIG
224700000000     C*
224800000000     C* PULIZIA FUNZIONI PRECEDENTI
224900000000     C                   MOVEL     *BLANKS       FUV(I)
225000000000     C*
225100000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
225200000000     C* (SALTA F24)
225300000000     C     FUDATT        IFEQ      'S'
225400000000     C     FUDFUN        ANDNE     '24'
225500000000     C                   ADD       1             W
225600000000     C                   MOVEL     FUDDES        FUV(W)
225700000000     C                   END
225800000000     C                   END                                                    DO WHILE
225900000000     C*
226000000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
226100000000     C* ALTRIMENTI DISATTIVO F24
226200000000     C     1             DO        24            I
226300000000     C                   MOVEL     FUD(I)        FUDRIG
226400000000     C*
226500000000     C     FUDFUN        IFEQ      '24'
226600000000     C     W             IFGT      6
226700000000     C                   MOVEL     'S'           FUDATT
226800000000     C                   ELSE
226900000000     C                   MOVEL     *BLANKS       FUDATT
227000000000     C                   END                                                    W GT 8
227100000000     C                   MOVEL     FUDRIG        FUD(I)
227200000000     C                   END
227300000000     C                   END                                                    DO WHILE
227400000000     C*
227500000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
227600000000     C     W             DIV       6             I
227700000000     C                   MVR                     Z
227800000000     C*
227900000000     C     Z             IFNE      0
228000000000     C     I             ADD       1             Z
228100000000     C                   ELSE
228200000000     C                   Z-ADD     I             Z                 5 0
228300000000     C                   END
228400000000     C*
228500000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
228600000000     C                   ADD       1             FUNGRU
228700000000     C     FUNGRU        IFGT      Z
228800000000     C     FUNGRU        ORLT      1
228900000000     C                   Z-ADD     1             FUNGRU
229000000000     C                   END
229100000000     C*
229200000000     C* PRIMO GRUPPO DI 6 FUNZIONI
229300000000     C     FUNGRU        IFEQ      1
229400000000     C                   MOVEL     FUV(1)        FUN001
229500000000     C                   MOVEL     FUV(2)        FUN002
229600000000     C                   MOVEL     FUV(3)        FUN003
229700000000     C                   MOVEL     FUV(4)        FUN004
229800000000     C                   MOVEL     FUV(5)        FUN005
229900000000     C                   MOVEL     FUV(6)        FUN006
230000000000     C                   END
230100000000     C*
230200000000     C* SECONDO GRUPPO DI 6 FUNZIONI
230300000000     C     FUNGRU        IFEQ      2
230400000000     C                   MOVEL     FUV(7)        FUN001
230500000000     C                   MOVEL     FUV(8)        FUN002
230600000000     C                   MOVEL     FUV(9)        FUN003
230700000000     C                   MOVEL     FUV(10)       FUN004
230800000000     C                   MOVEL     FUV(11)       FUN005
230900000000     C                   MOVEL     FUV(12)       FUN006
231000000000     C                   END
231100000000     C*
231200000000     C* TERZO GRUPPO DI 6 FUNZIONI
231300000000     C     FUNGRU        IFEQ      3
231400000000     C                   MOVEL     FUV(13)       FUN001
231500000000     C                   MOVEL     FUV(14)       FUN002
231600000000     C                   MOVEL     FUV(15)       FUN003
231700000000     C                   MOVEL     FUV(16)       FUN004
231800000000     C                   MOVEL     FUV(17)       FUN005
231900000000     C                   MOVEL     FUV(18)       FUN006
232000000000     C                   END
232100000000     C*
232200000000     C* QUARTO GRUPPO DI 6 FUNZIONI
232300000000     C     FUNGRU        IFEQ      4
232400000000     C                   MOVEL     FUV(19)       FUN001
232500000000     C                   MOVEL     FUV(20)       FUN002
232600000000     C                   MOVEL     FUV(21)       FUN003
232700000000     C                   MOVEL     FUV(22)       FUN004
232800000000     C                   MOVEL     *BLANKS       FUN005
232900000000     C                   MOVEL     *BLANKS       FUN006
233000000000     C                   END
233100000000     C*
233200000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
233300000000     C                   MOVEL     *BLANKS       FUN007
233400000000     C                   MOVEL     *BLANKS       FUN008
233500000000     C*
233600000000     C     1             DO        24            I
233700000000     C                   MOVEL     FUD(I)        FUDRIG
233800000000     C     FUDFUN        IFEQ      '24'
233900000000     C     FUDATT        ANDEQ     'S'
234000000000     C                   MOVEL     FUDDES        FUN008
234100000000     C                   END
234200000000     C                   END                                                    DO
234300000000     C                   ENDSR
234400000000     C*------------------------------------------------------------------------*
234500000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
234600000000     C*------------------------------------------------------------------------*
234700000000     C     FUNCON        BEGSR
234800000000     C*
234900000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
235000000000     C                   EXSR      FUNVAR
235100000000     C*
235200000000     C* PULISCO CAMPI DI LAVORO
235300000000     C                   Z-ADD     0             I                 5 0
235400000000     C                   Z-ADD     0             W                 5 0
235500000000     C*
235600000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
235700000000     C     FUNKEY        IFNE      *BLANKS
235800000000     C     1             DO        24            I
235900000000     C                   MOVEL     FUD(I)        FUDRIG
236000000000     C*
236100000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
236200000000     C     FUNKEY        IFEQ      FUDFUN
236300000000     C     FUDATT        ANDEQ     'S'
236400000000     C                   Z-ADD     I             W
236500000000     C                   END
236600000000     C                   END                                                    DO WHILE
236700000000     C*
236800000000     C* FUNZIONE NON ATTIVA (ERRORE)
236900000000     C     W             IFEQ      0
237000000000     C*
237100000000     C* EMISSIONE MESSAGGIO DI ERRORE
237200000000     C                   MOVEL     MSG(1)        DSMSMS
237300000000     C*
237400000000     C                   END                                                    W=0
237500000000     C                   END                                                    FUNKEY NE *BLANKS
237600000000     C*
237700000000     C* FUNZIONE ATTIVA (PROCEDI ...)
237800000000     C     W             IFNE      0
237900000000     C     FUNKEY        OREQ      *BLANKS
238000000000     C*
238100000000     C     FUNKEY        IFEQ      *BLANKS
238200000000     C                   EXSR      ENTGES
238300000000     C                   END
238400000000     C     FUNKEY        IFEQ      '03'
238500000000     C                   EXSR      F03GES
238600000000     C                   END
238700000000     C     FUNKEY        IFEQ      '06'
238800000000     C                   EXSR      F06GES
238900000000     C                   END
239000000000     C     FUNKEY        IFEQ      '12'
239100000000     C                   EXSR      F12GES
239200000000     C                   END
239300000000     C     FUNKEY        IFEQ      '24'
239400000000     C                   EXSR      F24GES
239500000000     C                   END
239600000000     C     FUNKEY        IFEQ      'HP'
239700000000     C                   EXSR      HELGES
239800000000     C                   END
239900000000     C*
240000000000     C                   END
240100000000     C*
240200000000     C                   ENDSR
240300000000     C*------------------------------------------------------------------------*
240400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
240500000000     C*------------------------------------------------------------------------*
240600000000     C     FUNVAR        BEGSR
240700000000     C*
240800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
240900000000     C*
241000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
241100000000     C     KEY           IFEQ      ENTER
241200000000     C                   MOVEL     *BLANKS       FUNKEY
241300000000     C                   END
241400000000     C     KEY           IFEQ      F1
241500000000     C                   MOVEL     '01'          FUNKEY
241600000000     C                   END
241700000000     C     KEY           IFEQ      F2
241800000000     C                   MOVEL     '02'          FUNKEY
241900000000     C                   END
242000000000     C     KEY           IFEQ      F3
242100000000     C                   MOVEL     '03'          FUNKEY
242200000000     C                   END
242300000000     C     KEY           IFEQ      F4
242400000000     C                   MOVEL     '04'          FUNKEY
242500000000     C                   END
242600000000     C     KEY           IFEQ      F5
242700000000     C                   MOVEL     '05'          FUNKEY
242800000000     C                   END
242900991103     C     KEY           IFEQ      F6
243000000000     C                   MOVEL     '06'          FUNKEY
243100000000     C                   END
243200000000     C     KEY           IFEQ      F7
243300000000     C                   MOVEL     '07'          FUNKEY
243400000000     C                   END
243500000000     C     KEY           IFEQ      F8
243600000000     C                   MOVEL     '08'          FUNKEY
243700000000     C                   END
243800000000     C     KEY           IFEQ      F9
243900000000     C                   MOVEL     '09'          FUNKEY
244000000000     C                   END
244100000000     C     KEY           IFEQ      F10
244200000000     C                   MOVEL     '10'          FUNKEY
244300000000     C                   END
244400000000     C     KEY           IFEQ      F11
244500000000     C                   MOVEL     '11'          FUNKEY
244600000000     C                   END
244700000000     C     KEY           IFEQ      F12
244800000000     C                   MOVEL     '12'          FUNKEY
244900000000     C                   END
245000000000     C     KEY           IFEQ      F13
245100000000     C                   MOVEL     '13'          FUNKEY
245200000000     C                   END
245300000000     C     KEY           IFEQ      F14
245400000000     C                   MOVEL     '14'          FUNKEY
245500000000     C                   END
245600000000     C     KEY           IFEQ      F15
245700000000     C                   MOVEL     '15'          FUNKEY
245800000000     C                   END
245900000000     C     KEY           IFEQ      F16
246000000000     C                   MOVEL     '16'          FUNKEY
246100000000     C                   END
246200000000     C     KEY           IFEQ      F17
246300000000     C                   MOVEL     '17'          FUNKEY
246400000000     C                   END
246500000000     C     KEY           IFEQ      F18
246600000000     C                   MOVEL     '18'          FUNKEY
246700000000     C                   END
246800000000     C     KEY           IFEQ      F19
246900000000     C                   MOVEL     '19'          FUNKEY
247000000000     C                   END
247100000000     C     KEY           IFEQ      F20
247200000000     C                   MOVEL     '20'          FUNKEY
247300000000     C                   END
247400000000     C     KEY           IFEQ      F21
247500000000     C                   MOVEL     '21'          FUNKEY
247600000000     C                   END
247700000000     C     KEY           IFEQ      F22
247800000000     C                   MOVEL     '22'          FUNKEY
247900000000     C                   END
248000000000     C     KEY           IFEQ      F23
248100000000     C                   MOVEL     '23'          FUNKEY
248200000000     C                   END
248300000000     C     KEY           IFEQ      F24
248400000000     C                   MOVEL     '24'          FUNKEY
248500000000     C                   END
248600000000     C     KEY           IFEQ      ROLLUP
248700000000     C                   MOVEL     'RU'          FUNKEY
248800000000     C                   END
248900000000     C     KEY           IFEQ      HELP
249000000000     C                   MOVEL     'HP'          FUNKEY
249100000000     C                   END
249200000000     C                   ENDSR
249300000000     C*------------------------------------------------------------------------*
249400000000     C* NOTE PER COMPILAZIONE SCHIERE
249500000000     C*------------------------------------------------------------------------*
249600000000     C* -MSG-
249700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
249800000000     C* (NON TOCCARE MSG,1)
249900000000     C*
250000000000     C* -CMD-
250100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
250200000000     C*
250300000000     C* -FUD-
250400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
250500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
250600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
250700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
250800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
250900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
251000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
251100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
251200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
251300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
251400000000     C*
251500000000     C*------------------------------------------------------------------------*
251600101112     OPRTF198   E            rigaErr_T   1
251700101112     O                       wInt1
251800061017     OPRTF198   E            rigaErr     1
251900061017     O                       wRiga
252000061023     C*------------------------------------------------------------------------*
252100101112     OPRTF1982  E            rigaSI95_T  1
252200101112     O                       wInt2
252300061120     OPRTF1982  E            rigaSI95    1
252400061023     O                       wSI95
252500061120     C*------------------------------------------------------------------------*
252600101112     OQSYSPRT   E            rigaSin_T   1
252700101112     O                       wInt3
252800061120     OQSYSPRT   E            rigaSin     1
252900061122     O                       d_dup_cap           +1
253000061120     O                       key_sin_loc         +1
253100061122     O                       d_dup_sin           +1
253200061120     O                       wrk_error           +1
253300991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
253400061017TISP20R- Tasto funzionale non gestito.                                         1
253500061017TISP20R-                                                                       2
253600061017TISP20R-                                                                       3
253700061017TISP20R-                                                                       4
253800061017TISP20R- Errori nella funzione di richiesta versioni. Avvertire il CED         5
253900061017TISP20R-                                                                       6
254000061017TISP20R-                                                                       7
254100061017TISP20R- Versione Cappario BAR inesistente.                                    8
254200061017TISP20R-                                                                       9
254300061017TISP20R- Versione obbligatoria, inserire                                      10
254400061017TISP20R-                                                                      11
254500061017TISP20R-                                                                      12
254600061017TISP20R-                                                                      13
254700061017TISP20R-                                                                      14
254800061017TISP20R-                                                                      15
254900061017TISP20R-                                                                      16
255000061017TISP20R-                                                                      17
255100061017TISP20R-                                                                      18
255200061017TISP20R-                                                                      19
255300061017TISP20R-                                                                      20
255400000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
255500000000                   -        (S/N)=N                        funz.01¬
255600000000                   -        (S/N)=N                        funz.02¬
255700000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
255800000000                   -        (S/N)=N                        funz.04¬
255900000000                   -        (S/N)=N                        funz.05¬
256000001120F06=Esecuzione     -        (S/N)=S                        funz.06¬
256100000104                   -        (S/N)=N                        funz.07¬
256200000104F08=Ins. Versione  -        (S/N)=N                        funz.08¬
256300000000                   -        (S/N)=N                        funz.09¬
256400000000                   -        (S/N)=N                        funz.10¬
256500000000                   -        (S/N)=N                        funz.11¬
256600991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
256700000000                   -        (S/N)=N                        funz.13¬
256800000000                   -        (S/N)=N                        funz.14¬
256900000000                   -        (S/N)=N                        funz.15¬
257000000000                   -        (S/N)=N                        funz.16¬
257100000000                   -        (S/N)=N                        funz.17¬
257200000000                   -        (S/N)=N                        funz.18¬
257300000000                   -        (S/N)=N                        funz.19¬
257400000000                   -        (S/N)=N                        funz.20¬
257500000000                   -        (S/N)=N                        funz.21¬
257600000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
257700000000                   -        (S/N)=N                        funz.23¬
257800991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
