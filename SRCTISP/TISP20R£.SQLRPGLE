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
053600140611     C*
053700140611     C* Verifico eventuale esistenza in cappario BRT
053800140611     C                   eval      K_cpcVER = NumVER
053900140611     C                   eval      K_cpcNAR = *blanks
054000140611     C                   eval      K_cpcCAP = NEWC_CAP
054100140611     C     KEYcpc01_C    chain     azcpc01l
054200140611     C                   if        not %found(azcpc01l)
054300101101     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
054400140611     C                             ' => CAP completamente NUOVO'+
054500140611     C                             '  ( gestire MANUALMENTE )'
054600140611     C                   else
054700101101     C                   except    rigaErr
054800140611     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
054900140611     C                             ' => CAP completamente NUOVO '+
055000140611     C                             'per PT - presente in cappario BRT ' +
055100140611     C                             '('+ cpcPRV +')' +
055200140611     C                             '  ( verificare e gestire MANUALMENTE )'
055300140611     C                   endif
055400101101     C*
055500101101     C* Se ELIM. CAP => Listo azione
055600101116     C                   when      OLDC_PRV<>*blanks AND OLDC_CAP<>*blanks AND
055700101116     C                             NEWC_PRV= *blanks AND NEWC_CAP= *blanks
055800101101     C                   eval      wRiga = OLDC_PRV + ' / ' + OLDC_CAP +
055900101102     C                             ' => presenti elim. totali o parziali  '+
056000101102     C                             '( verificare e gestire MANUALMENTE )'
056100101101     C                   except    rigaErr
056200101101     C*
056300101101     C* Se MODIFICA CAP => gestisco
056400101116     C                   when      OLDC_PRV<>*blanks AND OLDC_CAP<>*blanks AND
056500101116     C                             NEWC_PRV<>*blanks AND NEWC_CAP<>*blanks AND
056600101101     C                             (OLDC_PRV<>NEWC_PRV OR OLDC_CAP<>NEWC_CAP)
056700101101     C*
056800101101     C* Creo il nuovo cap duplicandolo dal suo precedente
056900101101     C                   eval      K_cpcVER = NumVER
057000101101     C                   eval      K_cpcNAR = *blanks
057100101101     C                   eval      K_cpcCAP = OLDC_CAP
057200101101     C     KEYcpc01_C    chain     azcpc01l
057300101101     C                   if        %found(azcpc01l)
057400101101     C*
057500101101     C* Verifico congruenza precisa provincie
057600101101     C                   if        OLDC_PRV <> cpcPRV AND OLDC_PRV <> cpcPRA
057700101101     C*
057800101101     C* Listo eventuale incongruenza
057900101101     C                   eval      wRiga = OLDC_PRV + ' / ' + OLDC_CAP +
058000110511     C                             ' => incongruenza dati BRT: '       +
058100101102     C                             cpcPRV + ' / ' + cpcCAP +
058200101102     C                             '  ( verificare MANUALMENTE )'
058300101101     C                   except    rigaErr
058400101101     C*
058500101101     C* Se tutto coincide => procedo
058600101101     C                   else
058700101101     C*
058800101101     C                   eval      cpcCAP = NEWC_CAP
058900101101     C                   eval      cpcPRV = NEWC_PRV
059000101101     C                   eval      cpcDTR = *all'7'
059100101101     C*
059200101101     C* Quindi inserisco il nuovo CAP
059300101116     C                   write(e)  azcpc000
059400101116     C*
059500101116     C* Listo azione
059600101116     C                   if        %error
059700101116     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
059800101116     C                             ' => fallita duplica da CAP: ' +
059900101116     C                             OLDC_PRV + ' / ' + OLDC_CAP
060000101116     C                   except    rigaErr
060100101116     C                   else
060200101101     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
060300101101     C                             ' => duplicato da CAP: ' +
060400101101     C                             OLDC_PRV + ' / ' + OLDC_CAP
060500101101     C                   except    rigaErr
060600101116     C                   endif
060700101101     C                   endif
060800101101     C                   endif
060900101101     C*
061000101101     C                   endsl
061100101101     C*
061200101101     C* Proseguo con la lettura
061300101101     C                   read      lnk_cap
061400101101     C                   enddo
061500101115     C*
061600101115     C* Scorro tutto il file variazioni loc poste x gestione modifiche loc / inseriimenti / cancellazioni
061700101115     C     *start        setll     lnk_loc
061800101115     C                   read      lnk_loc
061900101115     C                   dow       not %eof(lnk_loc)
062000101115     C*
062100101115     C                   select
062200101115     C*
062300101115     C* Se NUOVA LOC => Listo azione
062400101115     C                   when       OLDL_PRV =  *blanks   AND
062500101115     C                              OLDL_CAP =  *blanks   AND
062600101115     C                              OLDL_LOC =  *blanks   AND
062700101115     C                             (NEWL_PRV <> *blanks   OR
062800101115     C                              NEWL_CAP <> *blanks   OR
062900101115     C                              NEWL_LOC <> *blanks)
063000140611     C*
063100140611     C* Verifico eventuale esistenza in cappario BRT
063200140611     C                   eval      K_cplVER = NumVER
063300140611     C                   eval      K_cplNAR = *blanks
063400140611     C                   eval      K_cplCAP = NEWL_CAP
063500140611     C                   eval      K_cplLOC = NEWL_LOC
063600140611     C     KEYcpl01_C    chain     azcpl01l
063700140611     C                   if        not %found(azcpl01l)
063800140611     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
063900140611     C                             ' => LOC completamente NUOVA  '+
064000140611     C                             '( gestire MANUALMENTE )'
064100140611     C                   else
064200140611     C                   except    rigaErr
064300140611     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
064400140611     C                             ' => LOC completamente NUOVA  '+
064500140611     C                             'per PT - presente in cappario BRT' +
064600140611     C                             '  ( verificare e gestire MANUALMENTE )'
064700140611     C                   endif
064800101115     C                   except    rigaErr
064900101115     C*
065000101115     C* Se ELIM. LOC => Listo azione
065100101115     C                   when      (OLDL_PRV <> *blanks   OR
065200101115     C                              OLDL_CAP <> *blanks   OR
065300101115     C                              OLDL_LOC <> *blanks)  AND
065400101115     C                              NEWL_PRV =  *blanks   AND
065500101115     C                              NEWL_CAP =  *blanks   AND
065600101115     C                              NEWL_LOC =  *blanks
065700101115     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
065800101115     C                             ' => presenti elim. totali o parziali  '+
065900101115     C                             '( verificare e gestire MANUALMENTE )'
066000101115     C                   except    rigaErr
066100101115     C*
066200101115     C* Se MODIFICA LOC => gestisco
066300101115     C                   when      (OLDL_PRV <> *blanks   OR
066400101115     C                              OLDL_CAP <> *blanks   OR
066500101115     C                              OLDL_LOC <> *blanks)  AND
066600101115     C                             (NEWL_PRV <> *blanks   OR
066700101115     C                              NEWL_CAP <> *blanks   OR
066800101115     C                              NEWL_LOC <> *blanks)  AND
066900101115     C                             (NEWL_PRV <> OLDL_PRV  OR
067000101115     C                              NEWL_CAP <> OLDL_CAP  OR
067100101116     C                              NEWL_LOC <> OLDL_LOC) AND
067200101116     C                              OLDL_CAP <> *blanks   AND
067300101116     C                              OLDL_LOC <> *blanks   AND
067400101116     C                              NEWL_CAP <> *blanks   AND
067500101116     C                              NEWL_LOC <> *blanks
067600101115     C*
067700101115     C* Creo la nuova località duplicandola dalla sua precedente
067800101115     C                   eval      K_cplVER = NumVER
067900101115     C                   eval      K_cplNAR = *blanks
068000101115     C                   eval      K_cplCAP = OLDL_CAP
068100101115     C                   eval      K_cplLOC = OLDL_LOC
068200101115     C     KEYcpl01_C    chain     azcpl01l
068300101115     C                   if        %found(azcpl01l)
068400101116     C*
068500101116     C* Innanzitutto obsoletizzo la "vecchia" località
068600101116     C                   movel     'O'           cplFI1
068700101116     C                   movel     *blanks       cplFTR
068800101116     C                   eval      cplDTR = *all'8'
068900101116     C                   update(e) azcpl000
069000101116     C*
069100101116     C* Quindi inserisco la nuova località
069200101116     C                   movel     *blanks       cplFI1
069300101115     C                   eval      cplCAP = NEWL_CAP
069400101115     C                   eval      cplLOC = NEWL_LOC
069500101115     C                   eval      cplDTR = *all'7'
069600101115     C                   write(e)  azcpl000
069700101115     C*
069800101115     C* Listo azione
069900101115     C                   if        %error
070000101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
070100101115     C                                   ' => errore in inserimento nuova LOC'
070200101115     C                   except    rigaErr
070300101115     C                   else
070400101115     C*
070500101115     C* Se tutto ok => porto i sinonimi della "vecchia" località sulla nuova località
070600101115     C                   eval      K_cpsVER = NumVER
070700101115     C                   eval      K_cpsNAR = *blanks
070800101115     C                   eval      K_cpsCAP = OLDL_CAP
070900101115     C                   eval      K_cpsLOC = OLDL_LOC
071000101115     C                   eval      wCPSCAP  = NEWL_CAP
071100101115     C                   eval      wCPSLOC  = NEWL_LOC
071200101115     C***                exsr      movsin
071300101115     C                   exsr      dupsin
071400101115     C*
071500101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
071600101115     C                             ' => duplicata da LOC: ' +
071700101115     C                             OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC
071800101115     C                   except    rigaErr
071900101115     C                   endif
072000101115     C*
072100101115     C* Se vecchia località NON trovata => listo anomalia
072200101115     C                   else
072300101115     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
072400101115     C                                   ' => vecchia LOC non trovata, ' +
072500101115     C                                   'impossibile inserire nuova LOC: '+
072600101115     C                                   NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC
072700101115     C                   except    rigaErr
072800101115     C                   endif
072900101115     C*
073000101115     C                   endsl
073100101115     C*
073200101115     C* Proseguo con la lettura
073300101115     C                   read      lnk_loc
073400101115     C                   enddo
073500101101     C*
073600101101     C* <======================================================
073700061017     C*
073800061017     C* Scorro tutto il file cap poste
073900061017     C     *start        setll     cap_cap
074000061017     C                   read      cap_cap
074100061017     C                   dow       not %eof(cap_cap)
074200061017     C*
074300061017     C                   setoff                                       50
074400061017     C*
074500061017     C* Verifico se trattasi d cap già esistente
074600061017     C                   eval      K_cpcVER = NumVER
074700061017     C                   eval      K_cpcNAR = *blanks
074800061017     C                   eval      K_cpcCAP = c_cap
074900061017     C     KEYcpc01_C    chain     azcpc01l
075000061017     C                   if        %found(azcpc01l)
075100061024     C*
075200061024     C* Se provincia altrenativa già valorizzata e provincia cap PT corrente diversa da entrambe (prv e pra) => listo
075300061024     C                   if        c_prv  <> cpcPRV  AND
075400061024     C                             c_prv  <> cpcPRA  AND
075500061024     C                             c_prv  <> *blanks AND
075600061024     C                             cpcPRV <> *blanks AND
075700061024     C                             cpcPRA <> *blanks
075800101101     C                   eval      wRiga = cpcCAP + '  '    +
075900061024     C                             '=> tentativo scrittura 3 provincie ' +
076000061024     C                             '('+cpcPRV+' '+cpcPRA+' '+c_prv+')'
076100061024     C                   except    rigaErr
076200061024     C                   else
076300061017     C*
076400061017     C* Se cap già esistente verifico se è rimasto sulla stessa provincia
076500061017     C                   if        c_prv = *blanks OR
076600061017     C                             c_prv = cpcPRV  OR
076700061017     C                             c_prv = cpcPRA
076800061017     C                   seton                                        50
076900061017     C                   else
077000061017     C*
077100061017     C* Se provincia altrenativa già valorizzata => listo il cap x successiva verifica manuale
077200061024     C                   if        cpcPRA <> *blanks and cpcFTR <> *blanks
077300061017     C                   eval      wRiga = cpcCAP + '  '    +
077400061017     C                             '=> provincia alternativa sovrascritta ' +
077500061017     C                             '(' + cpcPRA + ')'
077600061017     C                   except    rigaErr
077700061017     C                   endif
077800061017     C*
077900061017     C* Quindi imposto la "vecchia" provincia come provincia alternativa ed imposto come provincia
078000061017     C* quella "nuova" PT
078100061017     C                   eval      cpcPRA = cpcPRV
078200061017     C                   eval      cpcPRV = c_prv
078300061017     C                   eval      cpcFTR = *blanks
078400080326     C                   eval      cpcDTR = *all'8'
078500061017     C                   update(e) azcpc000
078600061017     C                   seton                                        50
078700061017     C                   endif
078800061024     C*
078900061024     C                   endif
079000061017     C*
079100061017     C                   else
079200061017     C*
079300061017     C* Se cap inesistente flaggo apposito campo FLG2 su file cap PT
079400061017     C                   eval      c_FLG2 = 'N'
079500061017     C                   update    cap000
079600061017     C                   eval      wRiga = c_prv + '  '    +
079700061017     C                                     c_cap + '  '    +
079800061017     C                             '=> CAP inesistente!!!'
079900061017     C                   except    rigaErr
080000061017     C                   endif
080100061017     C*
080200061017     C* Se cap PT è stato in qualche modo trattato flaggo record su file PT
080300061017     C                   if        *in50 = *on
080400061017     C                   eval      c_FLG1 = 'S'
080500061017     C                   update(e) cap000
080600061017     C                   endif
080700061017     C*
080800061017     C* Proseguo con la lettura dei cap PT
080900061017     C                   read      cap_cap
081000061017     C                   enddo
081100061017     C*
081200061017     C                   ENDSR
081300061017     C*--------------------------------------------------------------------------------------------*
081400061017     C* exeloc - elaborazione località
081500061017     C*--------------------------------------------------------------------------------------------*
081600061017     C     exeloc        BEGSR
081700061017     C*
081800061017     C* "Normalizzo" le sintassi delle località BART
081900061017     C                   exsr      normloc
082000061017     C*
082100061017     C* Scorro tutto il file località poste
082200061017     C     *start        setll     cap_loc
082300061017     C                   read      cap_loc
082400061017     C                   dow       not %eof(cap_loc)
082500061017     C*
082600061017     C                   setoff                                       51
082700061017     C                   clear                   wfdcpl00
082800061023     C                   clear                   wCPLLIV
082900061017     C*
083000061017     C* 1° => se cap+loc PT trovati esattamente in file località BART (azcpl)
083100061017     C                   eval      K_cplVER = NumVER
083200061017     C                   eval      K_cplNAR = *blanks
083300061017     C                   eval      K_cplCAP = l_cap
083400061017     C                   eval      K_cplLOC = l_loc
083500061017     C     KEYcpl01_C    chain     azcpl01l
083600061017     C                   if        %found(azcpl01l)
083700080404     C                   if        cplFI1 <> *blanks OR
083800080404     C                             cplATB <> *blanks
083900080404     C                   eval      cplFI1 = *blanks
084000080404     C                   eval      cplATB = *blanks
084100080404     C                   eval      cplDTR = *all'6'
084200080404     C                   update    azcpl000
084300080404     C                   endif
084400080404     C*
084500061017     C* Se trovata esattamente => eseguo gestione sinonimi
084600061017     C                   exsr      exesin
084700061017     C                   seton                                        51
084800061023     C                   else
084900061023     C*
085000061023     C* Reperisco la provincia dal cap BART
085100061023     C                   eval      K_cpcVER = NumVER
085200061023     C                   eval      K_cpcNAR = *blanks
085300061023     C                   eval      K_cpcCAP = l_cap
085400061023     C     KEYcpc01_C    chain     azcpc01l
085500061023     C                   if        %found(azcpc01l)
085600061017     C*
085700061017     C* 2° => se cap+loc PT nn trovati esattamente in file località BART (azcpl)
085800061017     C*       verifico se la località è univoca nella provincia
085900061017     C                   exsr      rtvlocuniq
086000061017     C*
086100061023     C* Se località univoca nella provincia procedo con elaborazione particolare
086200061017     C                   if        uniq_cnt = 1
086300061017     C*
086400061017     C* Innanzitutto reperisce i dati della prv+loc corrente
086500061017     C                   exsr      rtvloc
086600061017     C*
086700061023     C* 1° obsoletizzo la località BART
086800061017     C                   eval      K_cplVER = loc_ver
086900061017     C                   eval      K_cplNAR = loc_nar
087000061017     C                   eval      K_cplCAP = loc_cap
087100061017     C                   eval      K_cplLOC = loc_loc
087200061017     C     KEYcpl01_C    chain     azcpl01l
087300061017     C                   if        %found(azcpl01l)
087400061023     C                   eval      wCPLLIV = cplLIV
087500061017     C                   movel     'O'           cplFI1
087600061017     C                   movel     *blanks       cplFTR
087700061017     C                   movel     cplESS        wCPLESS
087800061023     C***                movel     *blanks       cplESS
087900080326     C                   eval      cplDTR = *all'8'
088000061017     C                   update    azcpl000
088100061017     C* Tengo traccia della modifica nel file "diff" - Lato "OLD"
088200061017     C                   eval      wCPCVER = NumPrecVER
088300061017     C                   eval      wCPCCAP = cplCAP
088400061017     C                   exsr      rtvcap
088500061017     C                   eval      diff_cplNAR    = *blanks
088600061017     C                   eval      diff_cplLOC    = cplLOC
088700061017     C                   eval      diff_cpcPRVOLD = cpcPRV
088800061017     C                   eval      diff_cpcPRAOLD = cpcPRA
088900061017     C                   eval      diff_cplCAPOLD = cplCAP
089000061017     C*
089100061017     C                   endif
089200061017     C*
089300061017     C* 2° inserisco nuova loc+cap PT
089400061023     C*
089500061024     C* Se località "oldizzata" era a livello "C" => reperisco i dati d instradamento dal suo cap relativo
089600061024     C                   if        wCPLLIV = 'C'
089700061024     C                   eval      wCPCVER = loc_ver
089800061024     C                   eval      wCPCCAP = loc_cap
089900061024     C                   exsr      cpctocpl
090000061024     C                   endif
090100061023     C*
090200061017     C                   movel     *blanks       cplATB
090300061017     C                   movel     *blanks       cplFI1
090400061017     C                   movel     *blanks       cplFTR
090500061017     C                   movel     wCPLESS       cplESS
090600061023     C* Forzo sempre il nuovo inserimento a livello "L"
090700061023     C                   eval      cplLIV  = 'L'
090800061023     C*
090900061017     C                   eval      cplCAP  = l_cap
091000061017     C                   eval      cplLOC  = l_loc
091100061017     C                   eval      wCPCVER = NumVER
091200061017     C                   eval      wCPCCAP = l_cap
091300061017     C                   exsr      rtvcap
091400061017     C                   if        *in52 = *on
091500080326     C                   eval      cplDTR = *all'9'
091600061017     C                   write(e)  azcpl000
091700061024     C* Sempre se località "obsoletizzata" gestita a livello "C"  aggiorno il flag sul cap della località corrente
091800061024     C* che indica la presenza d eccezioni a livello d località
091900061024     C                   if        wCPLLIV = 'C' and cpcESL <> 'S'
092000061024     C                   eval      cpcESL = 'S'
092100080326     C                   eval      cpcDTR = *all'8'
092200061024     C                   update(e) azcpc000
092300061024     C                   endif
092400061017     C* Tengo traccia della modifica nel file "diff" - Lato "NEW"
092500061017     C                   eval      diff_cplNAR    = *blanks
092600061017     C                   eval      diff_cplLOC    = l_loc
092700061017     C                   eval      diff_cpcPRVNEW = cpcPRV
092800061017     C                   eval      diff_cpcPRANEW = cpcPRA
092900061017     C                   eval      diff_cplCAPNEW = cplCAP
093000061017     C                   write(e)  wfdcpl00
093100061017     C                   endif
093200061017     C*
093300061017     C* 3° porto i sinonimi della "vecchia" località sulla nuova località
093400061017     C                   eval      K_cpsVER = loc_ver
093500061017     C                   eval      K_cpsNAR = loc_nar
093600061017     C                   eval      K_cpsCAP = loc_cap
093700061017     C                   eval      K_cpsLOC = loc_loc
093800061017     C                   eval      wCPSCAP  = l_cap
093900061017     C                   eval      wCPSLOC  = l_loc
094000061023     C***                exsr      movsin
094100061023     C                   exsr      dupsin
094200061017     C*
094300061017     C* 4° inserisco i sinonimi relativi
094400061017     C                   exsr      exesin
094500061017     C*
094600061017     C                   seton                                        51
094700061017     C*
094800061017     C                   else
094900061017     C*
095000061017     C* Se località nn univoca nella provincia la inserisco a livello d cap
095100061017     C                   if        uniq_cnt > 1
095200061017     C                   exsr      insloc
095300061017     C* ...inserisco anche i sinonimi relativi
095400061017     C                   exsr      exesin
095500061017     C*                  eval      wRiga = l_prv + '  '    +
095600061017     C*                                    l_cap + '  '    +
095700061017     C*                                    l_loc + '     ' +
095800061017     C*                            '=> Località nn univoca nella provincia!'
095900061017     C*                  except    rigaErr
096000061017     C                   endif
096100061017     C*
096200061017     C* Se località nn trovata nella provincia la inserisco a livello d cap
096300061017     C                   if        uniq_cnt = 0
096400061017     C                   exsr      insloc
096500061017     C* ...inserisco anche i sinonimi relativi
096600061017     C                   exsr      exesin
096700061017     C*
096800061017     C*                  eval      wRiga = l_prv + '  '    +
096900061017     C*                                    l_cap + '  '    +
097000061017     C*                                    l_loc + '     ' +
097100061017     C*                            '=> Località nn trovata nella provincia!'
097200061017     C*                  except    rigaErr
097300061017     C                   endif
097400061017     C                   endif
097500061024     C*
097600061024     C                   else
097700061024     C                   eval      l_FLG2 = 'N'
097800061024     C                   update    loc000
097900061024     C                   eval      wRiga = l_prv + '  '    +
098000061024     C                                     l_cap + '  '    +
098100061024     C                                     l_loc + '     ' +
098200061024     C                             '=> Località su cap inesistente!'
098300061024     C                   except    rigaErr
098400061024     C                   endif
098500061017     C                   endif
098600061017     C*
098700061017     C* Se località PT è stata in qualche modo trattata flaggo record su file PT
098800061017     C                   if        *in51 = *on
098900061017     C                   eval      l_FLG1 = 'S'
099000061017     C                   update(e) loc000
099100061017     C                   endif
099200061017     C*
099300061017     C* Proseguo con la lettura delle località PT
099400061017     C                   read      cap_loc
099500061017     C                   enddo
099600061018     C*
099700061018     C                   EVAL      VID02 = 'Fatto'
099800061018     C                   SETOFF                                       92
099900061018     C                   WRITE     SP20D03
100000061017     C*
100100061017     C                   ENDSR
100200061017     C*--------------------------------------------------------------------------------------------*
100300061017     C* insloc - inserimento nuova loc BART da loc PT
100400061017     C*--------------------------------------------------------------------------------------------*
100500061017     C     insloc        BEGSR
100600061017     C*
100700061017     C                   clear                   azcpl000
100800061017     C                   movel     *blanks       cplATB
100900061017     C                   movel     *blanks       cplFI1
101000061017     C                   movel     *blanks       cplFTR
101100061017     C                   eval      cplVER = NumVER
101200061017     C                   eval      cplNAR = *blanks
101300061017     C                   eval      cplCAP = l_cap
101400061017     C                   eval      cplLOC = l_loc
101500061017     C                   eval      cplLIV = 'C'
101600061017     C                   eval      cplDLV = 'N'
101700061017     C                   eval      cplESS = 'S'
101800061017     C                   eval      wCPCVER = NumVER
101900061017     C                   eval      wCPCCAP = l_cap
102000061017     C                   exsr      rtvcap
102100061017     C                   if        *in52 = *on
102200080326     C                   eval      cplDTR = *all'9'
102300061017     C                   write(e)  azcpl000
102400061017     C                   else
102500061017     C                   eval      l_FLG2 = 'N'
102600061017     C                   update    loc000
102700061017     C                   eval      wRiga = l_prv + '  '    +
102800061017     C                                     l_cap + '  '    +
102900061017     C                                     l_loc + '     ' +
103000061017     C                             '=> Località su cap inesistente!'
103100061017     C                   except    rigaErr
103200061017     C                   endif
103300061017     C*
103400061017     C                   seton                                        51
103500061017     C*
103600061017     C                   ENDSR
103700061017     C*--------------------------------------------------------------------------------------------*
103800061017     C* exesin - gestione sinonimi
103900061017     C*--------------------------------------------------------------------------------------------*
104000061017     C     exesin        BEGSR
104100061017     C*
104200061017     C*
104300061017     C/EXEC SQL
104400061017     C+ declare C1 cursor for
104500061017     C+ select * from cap_sin
104600061017     C+ where PRV = :l_prv and CAP = :l_cap and LOC = :l_loc
104700061017     C+ order by PRV, CAP, LOC, SIN
104800061017     C+ for update of FLG1
104900061017     C/END-EXEC
105000061017     C
105100061017     C/EXEC SQL
105200061017     C+ open C1
105300061017     C/END-EXEC
105400061017     C
105500061017     C/EXEC SQL
105600061017     C+ Fetch next from C1 into :ds_cap_sin
105700061017     C/END-EXEC
105800061017     C*
105900061017     C* Se reperito qualcosa => analizzo
106000061017     C                   dow       sqlcod = *zeros
106100061017     C                   clear                   azcps000
106200061017     C                   eval      cpsVER = NumVER
106300061017     C                   eval      cpsNAR = *blanks
106400061017     C                   eval      cpsCAP = s_cap
106500061017     C                   eval      cpsLOC = s_loc
106600061017     C                   eval      cpsSIN = s_sin
106700061017     C                   eval      cpsFTR = *blanks
106800080326     C                   eval      cpsDTR = *all'9'
106900061017     C                   write(e)  azcps000
107000061017     C
107100061017     C/EXEC SQL
107200061017     C+     update cap_sin
107300061017     C+     set    FLG1 = 'S'
107400061017     C+     where  CURRENT  OF  C1
107500061017     C/END-EXEC
107600061017     C*
107700061017     C*
107800061017     C/EXEC SQL
107900061017     C+ Fetch next from C1 into :ds_cap_sin
108000061017     C/END-EXEC
108100061017     C*
108200061017     C                   enddo
108300061017     C*
108400061017     C/EXEC SQL
108500061017     C+ close C1
108600061017     C/END-EXEC
108700061017     C*
108800061017     C*
108900061017     C                   ENDSR
109000061017     C*--------------------------------------------------------------------------------------------*
109100061017     C* rtvlocuniq - reperimento univocità località x provincia
109200061017     C*--------------------------------------------------------------------------------------------*
109300061017     C     rtvlocuniq    BEGSR
109400061017     C*
109500061017     C/EXEC SQL
109600061017     C+ declare C2 cursor for
109700061024     C+ select cplloc, cpcprv, cpcpra, count(*)
109800061024     C+ from azcpc00f join azcpl00f on
109900061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
110000061024     C+ where cpcver = :NumVER and cpcnar = ' '
110100061024     C+ and (cpcprv = :cpcPRV or
110200061024     C+ (cpcprv = :cpcPRA and cpcprv<>'  ') or
110300061024     C+ cpcpra = :cpcPRV or
110400061024     C+ (cpcpra = :cpcPRA and cpcpra <> '  '))
110500061024     C+ and cplloc = :l_loc
110600061024     C+ group by cplloc, cpcprv, cpcpra
110700061017     C+ for read only
110800061017     C/END-EXEC
110900061017     C
111000061017     C/EXEC SQL
111100061017     C+ open C2
111200061017     C/END-EXEC
111300061017     C
111400061017     C/EXEC SQL
111500061017     C+ Fetch next from C2 into :ds_loc_uniq
111600061017     C/END-EXEC
111700061017     C*
111800061017     C* Se contatore > 1 => località nn univoca nella provincia
111900061017     C                   if        sqlcod = *zeros
112000061017     C                   else
112100061017     C                   eval      uniq_cnt = 0
112200061017     C                   endif
112300061017     C*
112400061017     C/EXEC SQL
112500061017     C+ close C2
112600061017     C/END-EXEC
112700061017     C*
112800061017     C*
112900061017     C                   ENDSR
113000061017     C*--------------------------------------------------------------------------------------------*
113100061017     C* rtvloc - reperimento località
113200061017     C*--------------------------------------------------------------------------------------------*
113300061017     C     rtvloc        BEGSR
113400061017     C*
113500061017     C/EXEC SQL
113600061017     C+ declare C3 cursor for
113700061017     C+ select cplver, cplnar, cplcap, cplloc
113800061017     C+ from azcpc00f join azcpl00f on
113900061017     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
114000061017     C+ where cpcver = :NumVER and cpcnar = ' '
114100061023     C+ and cpcprv = :uniq_prv and cpcpra = :uniq_pra
114200061023     C+ and cplloc = :l_loc
114300061017     C+ for read only
114400061017     C/END-EXEC
114500061017     C
114600061017     C/EXEC SQL
114700061017     C+ open C3
114800061017     C/END-EXEC
114900061017     C
115000061017     C/EXEC SQL
115100061017     C+ Fetch next from C3 into :ds_loc
115200061017     C/END-EXEC
115300061017     C
115400061017     C/EXEC SQL
115500061017     C+ close C3
115600061017     C/END-EXEC
115700061017     C*
115800061017     C                   ENDSR
115900061024     C*--------------------------------------------------------------------------------------------*
116000061024     C* rtvlocnew - reperimento nuova località (appena inserita da PT)
116100061024     C*--------------------------------------------------------------------------------------------*
116200061024     C     rtvlocnew     BEGSR
116300061024     C*
116400061024     C/EXEC SQL
116500061024     C+ declare C10 cursor for
116600061024     C+ select cplver, cplnar, cplcap, cplloc
116700061024     C+ from azcpc00f join azcpl00f on
116800061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
116900061024     C+ where cpcver = :NumVER and cpcnar = ' '
117000061024     C+ and cpcprv = :prv_prv and cpcpra = :prv_pra
117100061024     C+ and cplloc = :l2_cplLOC and cplfi1 = '  ' and cplftr = ' '
117200061024     C+ for read only
117300061024     C/END-EXEC
117400061024     C
117500061024     C/EXEC SQL
117600061024     C+ open C10
117700061024     C/END-EXEC
117800061024     C
117900061024     C/EXEC SQL
118000061024     C+ Fetch next from C10 into :ds_loc
118100061024     C/END-EXEC
118200061024     C
118300061024     C/EXEC SQL
118400061024     C+ close C10
118500061024     C/END-EXEC
118600061024     C*
118700061024     C                   ENDSR
118800061024     C*--------------------------------------------------------------------------------------------*
118900061024     C* rtvprv - reperimento provincia da cap + loc
119000061024     C*--------------------------------------------------------------------------------------------*
119100061024     C     rtvprv        BEGSR
119200061024     C*
119300061024     C/EXEC SQL
119400061024     C+ declare C9 cursor for
119500061024     C+ select cpcprv, cpcpra
119600061024     C+ from azcpc00f
119700061024     C+ where cpcver = :NumVER and cpcnar = ' '
119800061024     C+ and cpccap = :l2_cplCAP
119900061024     C+ for read only
120000061024     C/END-EXEC
120100061024     C
120200061024     C/EXEC SQL
120300061024     C+ open C9
120400061024     C/END-EXEC
120500061024     C
120600061024     C/EXEC SQL
120700061024     C+ Fetch next from C9 into :ds_prv
120800061024     C/END-EXEC
120900061024     C
121000061024     C/EXEC SQL
121100061024     C+ close C9
121200061024     C/END-EXEC
121300061024     C*
121400061024     C                   ENDSR
121500061017     C*--------------------------------------------------------------------------------------------*
121600061017     C* rtvcap - reperimento cap
121700061017     C*--------------------------------------------------------------------------------------------*
121800061017     C     rtvcap        BEGSR
121900061017     C*
122000061017     C                   setoff                                       52
122100061017     C*
122200061017     C* Verifico se trattasi d cap già esistente
122300061017     C                   eval      K_cpcVER = wCPCVER
122400061017     C                   eval      K_cpcNAR = *blanks
122500061017     C                   eval      K_cpcCAP = wCPCCAP
122600061017     C     KEYcpc01_C    chain     azcpc01l
122700061017     C                   if        %found(azcpc01l)
122800061017     C                   seton                                        52
122900061017     C                   endif
123000061017     C*
123100061017     C                   ENDSR
123200061017     C*--------------------------------------------------------------------------------------------*
123300061017     C* rtvcappt - reperimento cap PT
123400061017     C*--------------------------------------------------------------------------------------------*
123500061017     C     rtvcappt      BEGSR
123600061017     C*
123700061018     C                   setoff                                       54
123800061017     C*
123900061017     C/EXEC SQL
124000061017     C+ declare C4 cursor for
124100061017     C+ select * from cap_cap
124200061018     C+ where cap = :cpcCAP
124300061017     C+ for read only
124400061017     C/END-EXEC
124500061017     C
124600061017     C/EXEC SQL
124700061017     C+ open C4
124800061017     C/END-EXEC
124900061017     C
125000061017     C/EXEC SQL
125100061017     C+ Fetch next from C4 into :ds_cap_pt
125200061017     C/END-EXEC
125300061017     C*
125400061018     C* Se nn trovato => segnalo con indicatore 54
125500061017     C                   if        sqlcod <> *zeros
125600061018     C                   seton                                        54
125700061017     C                   endif
125800061017     C
125900061017     C/EXEC SQL
126000061017     C+ close C4
126100061017     C/END-EXEC
126200061017     C*
126300061017     C                   ENDSR
126400061017     C*--------------------------------------------------------------------------------------------*
126500061017     C* rtvlocpt - reperimento località PT
126600061017     C*--------------------------------------------------------------------------------------------*
126700061017     C     rtvlocpt      BEGSR
126800061017     C*
126900061017     C                   setoff                                       53
127000061017     C*
127100061017     C/EXEC SQL
127200061017     C+ declare C5 cursor for
127300061017     C+ select * from cap_loc
127400061024     C+ where cap = :cplCAP and substr(loc, 1, 35) = :cplLOC
127500061017     C+ for read only
127600061017     C/END-EXEC
127700061017     C
127800061017     C/EXEC SQL
127900061017     C+ open C5
128000061017     C/END-EXEC
128100061017     C
128200061017     C/EXEC SQL
128300061017     C+ Fetch next from C5 into :ds_loc_pt
128400061017     C/END-EXEC
128500061017     C*
128600061017     C* Se nn trovato => segnalo con indicatore 53
128700061017     C                   if        sqlcod <> *zeros
128800061017     C                   seton                                        53
128900061017     C                   endif
129000061017     C
129100061017     C/EXEC SQL
129200061017     C+ close C5
129300061017     C/END-EXEC
129400061017     C*
129500061017     C                   ENDSR
129600061017     C*--------------------------------------------------------------------------------------------*
129700061017     C* rtvcompt - reperimento comune PT
129800061017     C*--------------------------------------------------------------------------------------------*
129900061017     C     rtvcompt      BEGSR
130000061017     C*
130100061017     C                   Z-ADD     *zeros        com_cnt           9 0
130200061017     C*
130300061017     C/EXEC SQL
130400061017     C+ declare C6 cursor for
130500061017     C+ select com from cap_loc
130600061017     C+ where cap = :rainer_cap and loc = com
130700061017     C+ group by com
130800061017     C+ for read only
130900061017     C/END-EXEC
131000061017     C
131100061017     C/EXEC SQL
131200061017     C+ open C6
131300061017     C/END-EXEC
131400061017     C
131500061017     C/EXEC SQL
131600061017     C+ Fetch next from C6 into :ds_com_pt
131700061017     C/END-EXEC
131800061017     C*
131900061017     C* Se reperito qualcosa => analizzo e conto
132000061017     C                   dow       sqlcod = *zeros
132100061017     C                   add       1             com_cnt
132200061017     C*
132300061017     C/EXEC SQL
132400061017     C+ Fetch next from C6 into :ds_com_pt
132500061017     C/END-EXEC
132600061017     C*
132700061017     C                   enddo
132800061017     C*
132900061017     C/EXEC SQL
133000061017     C+ close C6
133100061017     C/END-EXEC
133200061017     C*
133300061017     C                   ENDSR
133400061017     C*--------------------------------------------------------------------------------------------*
133500061017     C* rtvlocrainer - reperimento località x provincia su file "Raineri"
133600061017     C*--------------------------------------------------------------------------------------------*
133700061017     C     rtvlocrainer  BEGSR
133800061017     C*
133900061017     C/EXEC SQL
134000061017     C+ declare C7 cursor for
134100061017     C+ select loc, min(cap), count(*)
134200061017     C+ from cap_rainer
134300061017     C+ where (prv = :cpcPRV or prv = :cpcPRA)
134400061024     C+ and substr(loc, 1, 35) = :cplLOC
134500061017     C+ group by loc
134600061017     C+ for read only
134700061017     C/END-EXEC
134800061017     C
134900061017     C/EXEC SQL
135000061017     C+ open C7
135100061017     C/END-EXEC
135200061017     C
135300061017     C/EXEC SQL
135400061017     C+ Fetch next from C7 into :ds_loc_rainer
135500061017     C/END-EXEC
135600061017     C*
135700061017     C* Se contatore > 1 => località nn univoca nella provincia
135800061017     C                   if        sqlcod = *zeros
135900061017     C                   else
136000061017     C                   eval      rainer_cnt = 0
136100061017     C                   endif
136200061017     C*
136300061017     C/EXEC SQL
136400061017     C+ close C7
136500061017     C/END-EXEC
136600061017     C*
136700061017     C*
136800061017     C                   ENDSR
136900061017     C*--------------------------------------------------------------------------------------------*
137000061017     C* movsin - spostamento sinonimi da "vecchia" località a "nuova" località
137100061017     C*--------------------------------------------------------------------------------------------*
137200061017     C     movsin        BEGSR
137300061017     C*
137400061017     C     KEYcps01_P    setll     azcps01l
137500061017     C                   if        %equal(azcps01l)
137600061017     C     KEYcps01_P    reade     azcps01l
137700061017     C                   dow       not %eof(azcps01l)
137800061017     C                   eval      cpsCAP = wCPSCAP
137900061017     C                   eval      cpsLOC = wCPSLOC
138000061017     C                   eval      cpsFTR = *blanks
138100061023     C* Solo se sinonimo corrente diverso da località sinonimata
138200061023     C                   if        cpsSIN <> cpsLOC
138300080326     C                   eval      cpsDTR = *all'8'
138400061017     C                   update(e) azcps000
138500061023     C                   endif
138600061017     C     KEYcps01_P    reade     azcps01l
138700061017     C                   enddo
138800061017     C                   endif
138900061017     C*
139000061017     C                   ENDSR
139100061017     C*--------------------------------------------------------------------------------------------*
139200061017     C* dupsin - copia sinonimi da "vecchia" località a "nuova" località
139300061017     C*--------------------------------------------------------------------------------------------*
139400061017     C     dupsin        BEGSR
139500061017     C*
139600061017     C     KEYcps01_P    setll     azcps01l
139700061017     C                   if        %equal(azcps01l)
139800061017     C     KEYcps01_P    reade     azcps01l
139900061017     C                   dow       not %eof(azcps01l)
140000061017     C                   eval      cpsCAP = wCPSCAP
140100061017     C                   eval      cpsLOC = wCPSLOC
140200061017     C                   eval      cpsFTR = *blanks
140300061023     C* Solo se sinonimo corrente diverso da località sinonimata
140400061023     C                   if        cpsSIN <> cpsLOC
140500080326     C                   eval      cpsDTR = *all'9'
140600061017     C                   write(e)  azcps000
140700061023     C                   endif
140800061017     C     KEYcps01_P    reade     azcps01l
140900061017     C                   enddo
141000061017     C                   endif
141100061017     C*
141200061017     C                   ENDSR
141300061017     C*--------------------------------------------------------------------------------------------*
141400061017     C* normloc - "normalizzazione" sintassi località BART
141500061017     C*--------------------------------------------------------------------------------------------*
141600061017     C     normloc       BEGSR
141700061017     C*
141800061017     C     NumVER        setll     azcpl01l
141900061017     C                   if        %equal(azcpl01l)
142000061017     C     NumVER        reade     azcpl01l
142100061017     C                   dow       not %eof(azcpl01l)
142200061017     C*
142300061017     C* Verifico presenza caratteri da eliminare
142400061017     C                   setoff                                       45
142500061017     C                   eval      wCPLLOC = cplLOC
142600061017     C                   if        %scan('''':cplLOC) > *zeros
142700061017     C     '''':' '      xlate     cplLOC        cplLOC
142800061017     C                   seton                                        45
142900061017     C                   endif
143000061017     C*
143100061017     C                   if        %scan('"':cplLOC)  > *zeros
143200061017     C     '"':' '       xlate     cplLOC        cplLOC
143300061017     C                   seton                                        45
143400061017     C                   endif
143500061017     C*
143600061017     C                   if        %scan('.':cplLOC)  > *zeros
143700061017     C     '.':' '       xlate     cplLOC        cplLOC
143800061017     C                   seton                                        45
143900061017     C                   endif
144000061017     C*
144100061017     C                   if        %scan('-':cplLOC)  > *zeros
144200061017     C     '-':' '       xlate     cplLOC        cplLOC
144300061017     C                   seton                                        45
144400061017     C                   endif
144500061017     C*
144600061017     C* Se sono state effettuate sostituzioni => gestisco sia località che sinonimi relativi
144700061017     C                   if        *in45 = *on
144800061017     C*
144900061017     C* Compatto gli eventuali spazi ripetuti consecutivamente
145000061017     C                   eval      cplLOC = %trim(cplLOC)
145100061017     C                   z-add     1             wPos              2 0
145200061017     C                   eval      wPos = %scan('  ':cplLOC:wPos)
145300061017     C                   dow       wPos > *zeros AND wPos < %len(%trim(cplLOC))
145400061017     C                   eval      cplLOC = %subst(cplLOC:1:wPos)+
145500061017     C                                      %subst(cplLOC:wPos+2)
145600061017     C                   eval      wPos = %scan('  ':cplLOC:wPos+1)
145700061017     C                   enddo
145800061017     C*
145900061017     C* 1° Sostituisco la località "normalizzata" a quella originale
146000061017     C                   eval      cplFTR = *blanks
146100080326     C                   eval      cplDTR = *all'8'
146200061017     C                   update(e) azcpl000
146300061017     C*
146400061017     C* 2° Inserisco come sinonimo della località "normalizzata" la località originale
146500061017     C                   clear                   azcps000
146600061017     C                   eval      cpsVER = cplVER
146700061017     C                   eval      cpsNAR = cplNAR
146800061017     C                   eval      cpsCAP = cplCAP
146900061017     C                   eval      cpsLOC = cplLOC
147000061017     C                   eval      cpsSIN = wCPLLOC
147100061017     C                   eval      cpsFTR = *blanks
147200080326     C                   eval      cpsDTR = *all'9'
147300061017     C                   write(e)  azcps000
147400061017     C*
147500061017     C* 3° Sposto i sinonimi della località originale su quella "normalizzata"
147600061017     C                   eval      K_cpsVER = cplVER
147700061017     C                   eval      K_cpsNAR = cplNAR
147800061017     C                   eval      K_cpsCAP = cplCAP
147900061017     C                   eval      K_cpsLOC = wCPLLOC
148000061017     C                   eval      wCPSCAP  = cplCAP
148100061017     C                   eval      wCPSLOC  = cplLOC
148200061018     C                   exsr      movsin
148300061017     C*
148400061017     C                   endif
148500061017     C*
148600061017     C* Proseguo con la lettura delle località BART
148700061017     C     NumVER        reade     azcpl01l
148800061017     C                   enddo
148900061017     C                   endif
149000061017     C*
149100061017     C                   ENDSR
149200061017     C*--------------------------------------------------------------------------------------------*
149300061017     C* exever1 - Esecuzione 1° verifica: cap BART no in cap PT => obsoletare cap BART
149400061017     C*--------------------------------------------------------------------------------------------*
149500061017     C     exever1       BEGSR
149600061018     C*
149700061018     C                   EVAL      VID03 = 'In elaborazione'
149800061018     C                   SETON                                        93
149900061018     C                   WRITE     SP20D03
150000061017     C*
150100061017     C* Scorro tutto il file dei cap BART
150200061018     C                   eval      K_cpcVER = NumVER
150300061018     C                   eval      K_cpcNAR = *blanks
150400061017     C     *start        setll     azcpc01l
150500061018     C     KEYcpc01_P    setll     azcpc01l
150600061018     C     KEYcpc01_P    reade     azcpc01l
150700061017     C                   dow       not %eof(azcpc01l)
150800061017     C*
150900061017     C* Se cap nn già obsoleto effettuo le verifiche
151000061018     C                   if        cpcFI1 = *blanks           AND
151100061018     C                             %subst(cpcCAP:4:2) <> '00' AND
151200061018     C                             %subst(cpcCAP:4:2) <> '  '
151300061017     C*
151400061017     C* Verifico se cap BART è in file cap PT
151500061018     C                   exsr      rtvcappt
151600061017     C*
151700061017     C* Se nn trovata flaggo in maniera "evidente" cap corrente BART
151800061018     C                   if        *in54 = *on
151900061018     C                   eval      cpcFI1 = '?'
152000080326     C                   eval      cpcDTR = *all'8'
152100061017     C                   update(e) azcpc000
152200061017     C                   endif
152300061017     C*
152400061017     C                   endif
152500061017     C*
152600061017     C* Proseguo con la lettura dei cap BART
152700061018     C     KEYcpc01_P    reade     azcpc01l
152800061017     C                   enddo
152900061018     C*
153000061018     C                   EVAL      VID03 = 'Fatto'
153100061018     C                   SETOFF                                       93
153200061018     C                   WRITE     SP20D03
153300061017     C*
153400061017     C                   ENDSR
153500061017     C*--------------------------------------------------------------------------------------------*
153600061017     C* exever2 - Esecuzione 2° verifica: loc BART no in loc PT => obsoletare loc BART
153700061017     C*--------------------------------------------------------------------------------------------*
153800061017     C     exever2       BEGSR
153900061018     C*
154000061018     C                   EVAL      VID04 = 'In elaborazione'
154100061018     C                   SETON                                        94
154200061018     C                   WRITE     SP20D03
154300061017     C*
154400061017     C* Scorro tutto il file delle località BART
154500061017     C     *start        setll     azcpl01l
154600061018     C     NumVER        setll     azcpl01l
154700061017     C     NumVER        reade     azcpl01l
154800061017     C                   dow       not %eof(azcpl01l)
154900061017     C*
155000061017     C                   setoff                                       60
155100061017     C                   setoff                                       61
155200061017     C*
155300061017     C* Se impostato flag "esistenza sinonimi" => verifica che sia vero
155400061017     C                   if        cplESS = 'S'
155500061017     C                   eval      cplESS = *blanks
155600061017     C                   seton                                        70
155700061017     C                   eval      K_cpsVER = NumVER
155800061017     C                   eval      K_cpsNAR = *blanks
155900061017     C                   eval      K_cpsCAP = cplCAP
156000061017     C                   eval      K_cpsLOC = cplLOC
156100061017     C     KEYcps01_P    setll     azcps01l
156200061017     C                   if        %equal(azcps01l)
156300061017     C     KEYcps01_P    reade     azcps01l
156400061017     C                   dow       not %eof(azcps01l)
156500061017     C                   if        cpsATB <> 'A'
156600061017     C                   eval      cplESS = 'S'
156700061017     C                   setoff                                       70
156800061017     C                   leave
156900061017     C                   endif
157000061017     C     KEYcps01_P    reade     azcps01l
157100061017     C                   enddo
157200061017     C                   endif
157300061017     C                   endif
157400061017     C*
157500061017     C* Se località nn già obsoleta effettuo le verifiche
157600061018     C                   if        cplFI1 = *blanks           AND
157700061018     C                             %subst(cplCAP:4:2) <> '00' AND
157800061018     C                             %subst(cplCAP:4:2) <> '  '
157900061017     C*
158000061017     C* Verifico se località BART presente in località PT
158100061017     C                   exsr      rtvlocpt
158200061017     C*
158300061017     C* Se nn trovata
158400061017     C                   if        *in53 = *on
158500061017     C                   seton                                        61
158600061017     C*
158700061017     C* Reperisco la provincia dal cap BART
158800061017     C                   eval      K_cpcVER = NumVER
158900061017     C                   eval      K_cpcNAR = *blanks
159000061017     C                   eval      K_cpcCAP = cplCAP
159100061017     C     KEYcpc01_C    chain     azcpc01l
159200061017     C                   if        %found(azcpc01l)
159300061017     C*
159400061017     C* Reperisco il cap PT dal file "Raineri" associato alla loc BART corrente
159500061017     C                   exsr      rtvlocrainer
159600061017     C*
159700061017     C* Se trovata e univoca procedo
159800061017     C                   if        rainer_cnt = 1
159900061017     C*
160000061017     C* Reperisco il comune PT associato al cap della loc BART corrente
160100061017     C                   exsr      rtvcompt
160200061017     C*
160300061017     C* Se trovata e univoco procedo
160400061017     C                   if        com_cnt = 1
160500061017     C*
160600061017     C                   seton                                        60
160700061017     C*
160800061017     C* Gestisco inserendo la località coerrente come sinonimo del comune cui nel file cap PT
160900061017     C* il cap della località BART corrente è associato, x cui =>
161000061017     C*
161100061017     C* 1° Annullo la località corrente
161200061017     C                   eval      cplFI1 = '!'
161300080326     C                   eval      cplDTR = *all'8'
161400061017     C                   update(e) azcpl000
161500061017     C*
161600061017     C* Inserisco come sinonimo del comune la località BART corrente (appena annullata)
161700061017     C                   clear                   azcps000
161800061017     C                   eval      cpsVER = cplVER
161900061017     C                   eval      cpsNAR = cplNAR
162000061017     C                   eval      cpsCAP = rainer_cap
162100061017     C                   eval      cpsLOC = com_com
162200061017     C                   eval      cpsSIN = cplLOC
162300061017     C                   eval      cpsFTR = *blanks
162400080326     C                   eval      cpsDTR = *all'9'
162500061017     C                   write(e)  azcps000
162600061017     C*
162700061017     C* Quindi sposto i sinonimi della località originale sul comune appena reperito
162800061017     C                   eval      K_cpsVER = cplVER
162900061017     C                   eval      K_cpsNAR = cplNAR
163000061017     C                   eval      K_cpsCAP = cplCAP
163100061017     C                   eval      K_cpsLOC = cplLOC
163200061017     C                   eval      wCPSCAP  = rainer_cap
163300061017     C                   eval      wCPSLOC  = com_com
163400061018     C***                exsr      movsin
163500061018     C                   exsr      dupsin
163600061017     C*
163700061017     C                   endif
163800061017     C                   endif
163900061017     C                   endif
164000061017     C*
164100061017     C* Se nn trovata flaggo in maniera "evidente" cap+loc corrente BART
164200061017     C                   if        *in60 = *off
164300061017     C                   eval      cplFI1 = '?'
164400061017     C                   endif
164500061017     C*
164600061017     C                   endif
164700061017     C*
164800061017     C                   endif
164900061017     C*
165000061017     C* Se occorre updatare => eseguo
165100061017     C                   if        (*in60 = *off AND *in61 = *on) OR
165200061017     C                             *in70
165300061017     C                   eval      cplFTR = *blanks
165400080326     C                   eval      cplDTR = *all'8'
165500061017     C                   update(e) azcpl000
165600061017     C                   endif
165700061017     C*
165800061017     C* Proseguo con la lettura delle località BART
165900061017     C     NumVER        reade     azcpl01l
166000061017     C                   enddo
166100061018     C*
166200061018     C                   EVAL      VID04 = 'Fatto'
166300061018     C                   SETOFF                                       94
166400061018     C                   WRITE     SP20D03
166500061017     C*
166600061017     C                   ENDSR
166700061017     C*--------------------------------------------------------------------------------------------*
166800061123     C* exever3 - Esecuzione 3° verifica: eliminare sin BART che sono già loc BART (nn obs) x lo stesso cap
166900061017     C*--------------------------------------------------------------------------------------------*
167000061017     C     exever3       BEGSR
167100061018     C*
167200061023     C                   EVAL      VID07 = 'In elaborazione'
167300061023     C                   SETON                                        97
167400080326     C                   WRITE     SP20D03
167500061017     C*
167600061017     C                   eval      wSQL =
167700061017     C                             'update azcps00f set cpsatb = ''A'', '    +
167800080326     C                             'cpsftr='' '', cpsdtr=88888888 '          +
167900080326     C                             ' where cpsver=' + VIDVER                 +
168000061017     C                             ' and cpscap!!cpssin in (select '         +
168100061017     C                             'cplcap!!cplloc from azcpl00f '           +
168200061017     C                             'where cplver=' + VIDVER + ' and cplFI1 ' +
168300061123     C                             '= '' '')'
168400061017     C*
168500061017     C/EXEC SQL
168600061017     C+ execute immediate: wSQL
168700061017     C/END-EXEC
168800061017     C*
168900061023     C                   EVAL      VID07 = 'Fatto'
169000061023     C                   SETOFF                                       97
169100061018     C                   WRITE     SP20D03
169200061018     C*
169300061017     C                   ENDSR
169400061017     C*------------------------------------------------------------------------*
169500061017     C* exever4 - Esecuzione 4° verifica: obsoleta cap BART aventi tutte relative loc BART obsolete
169600061017     C*------------------------------------------------------------------------*
169700061017     C     exever4       BEGSR
169800061018     C*
169900061018     C                   EVAL      VID06 = 'In elaborazione'
170000061018     C                   SETON                                        96
170100061018     C                   WRITE     SP20D03
170200061017     C*
170300061017     C/EXEC SQL
170400061017     C+ declare C8 cursor for select * from azcpl00f
170500061017     C+ where cplver = :NumVER and cplnar = ' '
170600061017     C+ order by cplcap, cplloc
170700061017     C+ for fetch only
170800061017     C/END-EXEC
170900061017     C
171000061017     C/EXEC SQL
171100061017     C+ open C8
171200061017     C/END-EXEC
171300061017     C
171400061017     C/EXEC SQL
171500061017     C+ Fetch C8 into :ds_cpl_obs
171600061017     C/END-EXEC
171700061017     C
171800061017     C                   dow       sqlcod=*zeros
171900061017     C*
172000061017     C* ESEGUO CONSIDERAZIONI ED OPERAZIONI DI SCRITTURA ENTRO CICLO
172100061017     C                   if        O_cplcap = savcap
172200061017     C                   if        O_cplfi1 = *blanks
172300061017     C                   movel     '0'           chkobs
172400061017     C                   endif
172500061017     C                   else
172600061017     C                   if        chkobs = '1' and savcap <> *blanks
172700061017     C                   eval      K_cpcVER = NumVER
172800061017     C                   eval      K_cpcNAR = *blanks
172900061017     C                   eval      K_cpcCAP = savcap
173000061017     C     KEYcpc01_C    chain     azcpc01l
173100061017     C                   if        %found(azcpc01l) AND cpcFI1 <> 'O'
173200061017     C                   movel     'O '          cpcFI1
173300061017     C                   movel     *blanks       cpcFTR
173400080326     C                   eval      cpcDTR = *all'8'
173500061017     C                   update(e) azcpc000
173600061017     C                   endif
173700061017     C                   endif
173800061017     C                   movel     O_cplcap      savcap
173900061017     C                   movel     '1'           chkobs
174000061017     C                   if        O_cplfi1 = *blanks
174100061017     C                   movel     '0'           chkobs
174200061017     C                   endif
174300061017     C                   endif
174400061017     C
174500061017     C/EXEC SQL
174600061017     C+ Fetch C8 into :ds_cpl_obs
174700061017     C/END-EXEC
174800061017     C
174900061017     C                   enddo
175000061017     C
175100061017     C/EXEC SQL
175200061017     C+ close C8
175300061017     C/END-EXEC
175400061018     C*
175500061018     C                   EVAL      VID06 = 'Fatto'
175600061018     C                   SETOFF                                       96
175700061018     C                   WRITE     SP20D03
175800061017     C*
175900061017     C                   ENDSR
176000061023     C*--------------------------------------------------------------------------------------------*
176100061023     C* exever5 - Esecuzione 5° verifica: vaglia loc BART "?" tramite TISI95R
176200061023     C*--------------------------------------------------------------------------------------------*
176300061023     C     exever5       BEGSR
176400061023     C*
176500061023     C                   EVAL      VID05 = 'In elaborazione'
176600061023     C                   SETON                                        95
176700061023     C                   WRITE     SP20D03
176800061023     C*
176900061023     C* Scorro tutto il file delle località BART
177000061023     C     *start        setll     azcpl01l
177100061023     C     NumVER        setll     azcpl01l
177200061023     C     NumVER        reade     azcpl01l
177300061023     C                   dow       not %eof(azcpl01l)
177400061024     C*
177500061024     C                   setoff                                       58
177600061023     C*
177700061023     C* Se località già battezzata "?" elaboro
177800061023     C                   if        %trim(cplFI1) = '?'
177900061023     C*
178000061023     C* Reperisco la provincia dal cap BART
178100061023     C                   eval      K_cpcVER = NumVER
178200061023     C                   eval      K_cpcNAR = *blanks
178300061023     C                   eval      K_cpcCAP = cplCAP
178400061023     C     KEYcpc01_C    chain     azcpc01l
178500061023     C                   if        %found(azcpc01l)
178600061023     C*
178700061023     C* Passo al TISI95R loc + prv e se livello affidabilità >= 2 listo e tengo traccia aggiornando la località
178800061023     C                   clear                   TISI95DS
178900061023     C                   eval      I95TCN = '7'
179000061023     C                   z-add     datcor        I95DAT
179100061023     C                   eval      I95LOC = cplLOC
179200061023     C                   eval      I95PRV = cpcPRV
179300061023     C                   call      'TISI95R'
179400061023     C                   parm                    TISI95DS
179500061023     C*
179600061023     C                   if        O95LIA >= '2'
179700061024     C*
179800061024     C                   seton                                        58
179900061023     C*
180000061023     C* 1° Listo in stampa x preventiva verifica manuale
180100061023     C                   eval      wSI95 = 'DA: ' +
180200061023     C                                     cplCAP + ' ' +
180300061023     C                                     cplLOC + ' ' +
180400061023     C                                     cpcPRV + ' ' +
180500061023     C                                     ' => A: ' +
180600061023     C                                     O95CAP + ' ' +
180700061023     C                                     O95LOC + ' ' +
180800061023     C                                     O95PRV + ' ' +
180900061023     C                                     O95SIN + ' ' +
181000061023     C                                     O95LID + ' ' +
181100061023     C                                     O95LIA
181200061023     C                   except    rigaSI95
181300061023     C*
181400061023     C* 2° Marco la località corrente (solo se lancio no in simulazione)
181500061023     C                   if        VIDSI95 = 'X'
181600061023     C                   eval      cplFI1  = 'T'
181700080326     C                   eval      cplDTR = *all'8'
181800061023     C                   update(e) azcpl000
181900061023     C*
182000061023     C* 3° Sposto i sinonimi della località originale sul comune appena reperito
182100061023     C                   eval      K_cpsVER = cplVER
182200061023     C                   eval      K_cpsNAR = cplNAR
182300061023     C                   eval      K_cpsCAP = cplCAP
182400061023     C                   eval      K_cpsLOC = cplLOC
182500061204     C                   eval      wCPSCAP  = O95CAP
182600061204     C                   eval      wCPSLOC  = O95LOC
182700061023     C***                exsr      movsin
182800061024     C                   exsr      dupsin
182900061023     C                   endif
183000061023     C*
183100061023     C                   endif
183200061023     C                   endif
183300061023     C                   endif
183400061024     C*
183500061024     C* Se x qualunque ragione nn ho elaborato => listo
183600061024     C***                if        *in58 = *off
183700061024     C***                eval      wSI95 = 'DA: ' +
183800061024     C***                                  cplCAP + ' ' +
183900061024     C***                                  cplLOC + ' ' +
184000061024     C***                                  cpcPRV + ' ' +
184100061024     C***                                  ' => N.P. '
184200061024     C***                except    rigaSI95
184300061024     C***                endif
184400061023     C*
184500061023     C* Proseguo con la lettura delle località BART
184600061023     C     NumVER        reade     azcpl01l
184700061023     C                   enddo
184800061023     C*
184900061023     C                   EVAL      VID05 = 'Fatto'
185000061023     C                   SETOFF                                       95
185100061023     C                   WRITE     SP20D03
185200061023     C*
185300061023     C                   ENDSR
185400061024     C*--------------------------------------------------------------------------------------------*
185500061024     C* exever6 - Esecuzione 6° verifica: sistema le LOC BART obsolete e a livello "C" x
185600061024     C*                                   ricongiunzione a nuova località su diverso cap  ma in
185700061024     C*                                   stessa provincia / provincia alternativa
185800061024     C*--------------------------------------------------------------------------------------------*
185900061024     C     exever6       BEGSR
186000061024     C*
186100061024     C                   EVAL      VID07 = 'In elaborazione'
186200061024     C                   SETON                                        97
186300061024     C                   WRITE     SP20D03
186400061024     C*
186500061024     C* Scorro tutto il file delle località BART
186600061024     C     *start        setll     azcpl02l
186700061024     C     NumVER        setll     azcpl02l
186800061024     C     NumVER        reade     azcpl02l
186900061024     C                   dow       not %eof(azcpl02l)
187000061024     C*
187100061024     C* Elaboro località obsolete e a livello "C"
187200061024     C                   if        l2_cplFI1 = 'O ' AND
187300061024     C                             l2_cplLIV = 'C'
187400061024     C*
187500061024     C* Reperisco innanzitutto la provincia / prov alternativa della località corrente
187600061024     C                   clear                   ds_prv
187700061024     C                   exsr      rtvprv
187800061024     C*
187900061024     C* Se reperito qualcosa proseguo con elaborazione
188000061024     C                   if        ds_prv <> *blanks
188100061024     C*
188200061024     C* Quindi reperisco la località precisa
188300061024     C                   clear                   ds_loc
188400061024     C                   exsr      rtvlocnew
188500061024     C*
188600061024     C* Se reperito qualcosa proseguo con elaborazione
188700061024     C                   if        ds_loc <> *blanks
188800061024     C*
188900061024     C* Quindi aggancio la nuova località inserita da PT
189000061024     C                   eval      K_cplVER = loc_ver
189100061024     C                   eval      K_cplNAR = loc_nar
189200061024     C                   eval      K_cplCAP = loc_cap
189300061024     C                   eval      K_cplLOC = loc_loc
189400061024     C     KEYcpl01_C    chain     azcpl01l
189500061024     C                   if        %found(azcpl01l)
189600061024     C*
189700061024     C* Reperisco i dati d instradamento dal cap della relativa località "obsoletizzata"
189800061024     C                   eval      wCPCVER = l2_cplVER
189900061024     C                   eval      wCPCCAP = l2_cplCAP
190000061024     C                   exsr      cpctocpl
190100061024     C*
190200061024     C* Imposto quindi a "L" il livello d gestioine della nuova località inserita da PT
190300061024     C                   eval      cplLIV  = 'L'
190400061024     C*
190500061024     C* Superfluo ma cmq sfleggo il record
190600061024     C                   movel     *blanks       cplFTR
190700061024     C*
190800061024     C* Quindi aggiorno il record della nuova località inserita da PT
190900080326     C                   eval      cplDTR = *all'8'
191000061024     C                   update(e) azcpl000
191100061024     C*
191200061024     C* A questo punto occorre indicare sul cap della nuova localita inserita da PT la presenza d eccezioni x localita
191300061024     C                   eval      wCPCVER = loc_ver
191400061024     C                   eval      wCPCCAP = loc_cap
191500061024     C                   exsr      rtvcap
191600061024     C                   if        *in52 = *on
191700061024     C                   eval      cpcESL = 'S'
191800080326     C                   eval      cpcDTR = *all'8'
191900061024     C                   update(e) azcpc000
192000061024     C                   endif
192100061024     C                   endif
192200061024     C                   endif
192300061024     C                   endif
192400061024     C                   endif
192500061024     C*
192600061024     C* Proseguo con la lettura delle località BART
192700061024     C     NumVER        reade     azcpl02l
192800061024     C                   enddo
192900061024     C*
193000061024     C                   EVAL      VID07 = 'Fatto'
193100061024     C                   SETOFF                                       97
193200061024     C                   WRITE     SP20D03
193300061024     C*
193400061122     C                   ENDSR
193500061122     C*--------------------------------------------------------------------------------------------*
193600061122     C* exever8 - Esecuzione 8° verifica: annullaamento sinonimi doppi su stesso cap
193700061122     C*--------------------------------------------------------------------------------------------*
193800061122     C     exever8       BEGSR
193900061122     C*
194000061122     C                   EVAL      VID08 = 'In elaborazione'
194100061122     C                   SETON                                        98
194200061122     C                   WRITE     SP20D03
194300061122     C*
194400061122     C* Inizializzo il contatore d stesso sin su stesso cap
194500061122     C                   eval      w_cnt_sin = *zeros
194600061122     C*
194700061122     C/EXEC SQL
194800061122     C+ declare C11 cursor for
194900061122     C+ select cplfi1, cpscap, cpssin, cpsloc from azcps00f join azcpl00f
195000061122     C+ on  cpsver=cplver and cpsnar=cplnar and cpscap=cplcap
195100061122     C+ and cpsloc=cplloc
195200061123     C+ where cpsver=:NumVER
195300061122     C+ order by cpscap, cpssin, cplfi1
195400061122     C+ for read only
195500061122     C/END-EXEC
195600061122     C
195700061122     C/EXEC SQL
195800061122     C+ open C11
195900061122     C/END-EXEC
196000061122     C
196100061122     C/EXEC SQL
196200061122     C+ Fetch next from C11 into :ds_dup_sin
196300061122     C/END-EXEC
196400061122     C*
196500061122     C* Se reperito qualcosa => analizzo
196600061122     C                   dow       sqlcod = *zeros
196700061122     C*
196800061122     C* Verifico se già presente x tesso cap
196900061122     C                   if        d_dup_cap = cnt_sin_cap AND
197000061122     C                             d_dup_sin = cnt_sin_sin
197100061122     C*
197200061122     C* Incremento il contatore d stesso sin su stesso cap
197300061122     C                   eval      w_cnt_sin = w_cnt_sin + 1
197400061122     C*
197500061122     C* Se contatore > 1 => annullo il sin corrente
197600061122     C                   if        w_cnt_sin > 1
197700061122     C                   eval      key_sin_loc = d_dup_loc
197800061122     C                   exsr      annSIN
197900061122     C                   endif
198000061122     C*
198100061122     C* Se nn già presente salvo cap/sin correnti
198200061122     C                   else
198300061122     C*
198400061122     C* Re-imposto a 1 il contatore d sin x stesso cap
198500061122     C                   eval      w_cnt_sin = 1
198600061122     C*
198700061122     C* Quindi salvo il cap/sin corrente
198800061122     C                   eval      cnt_sin_cap = d_dup_cap
198900061122     C                   eval      cnt_sin_sin = d_dup_sin
199000061122     C                   endif
199100061122     C*
199200061122     C* Proseguo con la lettura dei sin
199300061122     C*
199400061122     C/EXEC SQL
199500061122     C+ Fetch next from C11 into :ds_dup_sin
199600061122     C/END-EXEC
199700061122     C*
199800061122     C                   enddo
199900061122     C*
200000061122     C/EXEC SQL
200100061122     C+ close C11
200200061122     C/END-EXEC
200300061122     C*
200400061122     C                   EVAL      VID08 = 'Fatto'
200500061122     C                   SETOFF                                       98
200600061122     C                   WRITE     SP20D03
200700061122     C*
200800061122     C                   ENDSR
200900061120     C*--------------------------------------------------------------------------------------------*
201000061120     C* annSIN - annulla sinonimo
201100061120     C*--------------------------------------------------------------------------------------------*
201200061120     C     annSIN        BEGSR
201300061120     C*
201400061120     C                   eval      K_cpsVER = NumVER
201500061120     C                   eval      K_cpsNAR = *blanks
201600061122     C                   eval      K_cpsCAP = d_dup_cap
201700061120     C                   eval      K_cpsLOC = key_sin_loc
201800061122     C                   eval      K_cpsSIN = d_dup_sin
201900061120     C     KEYcps01_C    chain(e)  azcps01l
202000061120     C                   if        %error
202100061120     C                   eval      wrk_error = 'E'
202200061120     C                   except    rigaSin
202300061120     C                   else
202400061120     C                   if        %found(azcps01l)
202500061120     C                   eval      cpsATB = 'A'
202600061120     C                   eval      cpsFTR = *blanks
202700080326     C                   eval      cpsDTR = *all'8'
202800061120     C                   update    azcps000
202900061120     C                   eval      wrk_error = *blanks
203000061120     C                   except    rigaSin
203100061120     C                   endif
203200061120     C                   endif
203300061120     C*
203400061120     C                   ENDSR
203500061024     C*--------------------------------------------------------------------------------------------*
203600061024     C* updFCPU - aggiornamento provincie alternative
203700061024     C*--------------------------------------------------------------------------------------------*
203800061024     C     updFCPU       BEGSR
203900061024     C*
204000061024     C                   EVAL      VID02 = 'In elaborazione'
204100061024     C                   SETON                                        92
204200061024     C                   WRITE     SP20D03
204300061024     C*
204400061024     C                   eval      wSQL = 'update azcpc00f ' +
204500061024     C                             'set cpcPRV=''FC'', cpcPRA=''FO'' ' +
204600061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''FO'' '+
204700061024     C                             'and cpcPRA=''FC'''
204800061024     C*
204900061024     C/EXEC SQL
205000061024     C+ execute immediate: wSQL
205100061024     C/END-EXEC
205200061024     C*
205300061024     C                   eval      wSQL = 'update azcpc00f ' +
205400061024     C                             'set cpcPRV=''PU'', cpcPRA=''PS'' ' +
205500061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''PS'' '+
205600061024     C                             'and cpcPRA=''PU'''
205700061024     C*
205800061024     C/EXEC SQL
205900061024     C+ execute immediate: wSQL
206000061024     C/END-EXEC
206100061024     C*
206200061024     C                   ENDSR
206300061017     C*--------------------------------------------------------------------------------------------*
206400061017     C* inzpt - inizializzazione flag di elaborazione file PT
206500061017     C*--------------------------------------------------------------------------------------------*
206600061017     C     inzpt         BEGSR
206700061018     C*
206800061018     C                   EVAL      VID01 = 'In elaborazione'
206900061018     C                   SETON                                        91
207000061018     C                   WRITE     SP20D03
207100061017     C*
207200061017     C                   eval      wSQL = 'update cap_cap set FLG1 = '' '', ' +
207300061017     C                                                       'FLG2 = '' '''
207400061017     C*
207500061017     C/EXEC SQL
207600061017     C+ execute immediate: wSQL
207700061017     C/END-EXEC
207800061017     C*
207900061017     C                   eval      wSQL = 'update cap_loc set FLG1 = '' '', ' +
208000061017     C                                                       'FLG2 = '' '''
208100061017     C*
208200061017     C/EXEC SQL
208300061017     C+ execute immediate: wSQL
208400061017     C/END-EXEC
208500061017     C                   eval      wSQL = 'update cap_sin set FLG1 = '' '', ' +
208600061017     C                                                       'FLG2 = '' '''
208700061017     C*
208800061017     C/EXEC SQL
208900061017     C+ execute immediate: wSQL
209000061017     C/END-EXEC
209100061017     C*
209200061017     C                   eval      wSQL = 'delete from azcpc00f where cpcver='+
209300061017     C                                    VIDVER
209400061017     C*
209500061017     C/EXEC SQL
209600061017     C+ execute immediate: wSQL
209700061017     C/END-EXEC
209800061017     C*
209900061017     C                   eval      wSQL = 'delete from azcpl00f where cplver='+
210000061017     C                                    VIDVER
210100061017     C*
210200061017     C/EXEC SQL
210300061017     C+ execute immediate: wSQL
210400061017     C/END-EXEC
210500061017     C                   eval      wSQL = 'delete from azcps00f where cpsver='+
210600061017     C                                    VIDVER
210700061017     C*
210800061017     C/EXEC SQL
210900061017     C+ execute immediate: wSQL
211000061017     C/END-EXEC
211100061017     C*
211200061017     C                   CALL      'TISP20C1'
211300061017     C*
211400061017     C                   eval      wSQL = 'update azcpc00f set cpcFTR = ''T'' '+
211500061017     C                                    'where cpcver='+VIDVER
211600061017     C*
211700061017     C/EXEC SQL
211800061017     C+ execute immediate: wSQL
211900061017     C/END-EXEC
212000061017     C*
212100061017     C                   eval      wSQL = 'update azcpl00f set cplFTR = ''T'' '+
212200061017     C                                    'where cplver='+VIDVER
212300061017     C*
212400061017     C/EXEC SQL
212500061017     C+ execute immediate: wSQL
212600061017     C/END-EXEC
212700061017     C                   eval      wSQL = 'update azcps00f set cpsFTR = ''T'' '+
212800061017     C                                    'where cpsver='+VIDVER
212900061017     C*
213000061017     C/EXEC SQL
213100061017     C+ execute immediate: wSQL
213200061017     C/END-EXEC
213300061018     C*
213400061018     C                   EVAL      VID01 = 'Fatto'
213500061018     C                   SETOFF                                       91
213600061018     C                   WRITE     SP20D03
213700061017     C*
213800061017     C                   ENDSR
213900061024     C*--------------------------------------------------------------------------------------------*
214000061024     C* cpctocpl - passaggio dati da livello "C" a "L"
214100061024     C*--------------------------------------------------------------------------------------------*
214200061024     C     cpctocpl      BEGSR
214300061024     C*
214400061024     C                   exsr      rtvcap
214500061024     C                   if        *in52 = *on
214600061024     C                   eval      cplLNA = cpcLNA
214700061024     C                   eval      cplZNC = cpcZNC
214800061024     C                   eval      cplISO = cpcISO
214900061024     C                   eval      cplZFD = cpcZFD
215000061024     C                   eval      cplLKG = cpcLKG
215100061024     C                   eval      cplLMC = cpcLMC
215200061024     C                   eval      cplLOL = cpcLOL
215300061024     C                   eval      cplZOL = cpcZOL
215400061024     C                   eval      cplLKS = cpcLKS
215500061024     C                   eval      cplLMS = cpcLMS
215600061024     C                   eval      cplLOS = cpcLOS
215700061024     C                   eval      cplZOS = cpcZOS
215800061024     C                   eval      cplLK2 = cpcLK2
215900061024     C                   eval      cplLM2 = cpcLM2
216000061024     C                   eval      cplLO2 = cpcLO2
216100061024     C                   eval      cplZO2 = cpcZO2
216200061024     C                   eval      cplCTS = cpcCTS
216300061024     C                   eval      cplCTE = cpcCTE
216400061024     C                   eval      cplTTC = cpcTTC
216500061024     C                   eval      cplPII = cpcPII
216600061024     C                   eval      cplPID = cpcPID
216700061024     C                   eval      cplPIS = cpcPIS
216800061024     C                   eval      cplGDD = cpcGDD
216900061121     C                   eval      cplFI2 = cpcFI2
217000061024     C                   endif
217100061024     C*
217200061024     C                   ENDSR
217300000000     C*------------------------------------------------------------------------*
217400980403     C* *INZSR - OPERAZIONI INIZIALI
217500000000     C*------------------------------------------------------------------------*
217600980403     C     *INZSR        BEGSR
217700000000     C*
217800000000     C* RICEVIMENTO PARAMETRI
217900000000     C     *ENTRY        PLIST
218000000000     C                   PARM                    KPJBA
218100061017     C*
218200061017     C* Definizione chiavi di lettura
218300061017     C*
218400061017     C* AZCPC01L - Completa
218500061017     C     KEYcpc01_C    KLIST
218600061017     C                   KFLD                    K_cpcVER
218700061017     C                   KFLD                    K_cpcNAR
218800061017     C                   KFLD                    K_cpcCAP
218900061018     C*
219000061018     C* AZCPC01L - Parziale
219100061018     C     KEYcpc01_P    KLIST
219200061018     C                   KFLD                    K_cpcVER
219300061018     C                   KFLD                    K_cpcNAR
219400061017     C*
219500061017     C* AZCPL01L - Completa
219600061017     C     KEYcpl01_C    KLIST
219700061017     C                   KFLD                    K_cplVER
219800061017     C                   KFLD                    K_cplNAR
219900061017     C                   KFLD                    K_cplCAP
220000061017     C                   KFLD                    K_cplLOC
220100061017     C*
220200061017     C* AZCPS01L - Parziale
220300061017     C     KEYcps01_P    KLIST
220400061017     C                   KFLD                    K_cpsVER
220500061017     C                   KFLD                    K_cpsNAR
220600061017     C                   KFLD                    K_cpsCAP
220700061017     C                   KFLD                    K_cpsLOC
220800061120     C*
220900061120     C* AZCPS01L - Completa
221000061120     C     KEYcps01_C    KLIST
221100061120     C                   KFLD                    K_cpsVER
221200061120     C                   KFLD                    K_cpsNAR
221300061120     C                   KFLD                    K_cpsCAP
221400061120     C                   KFLD                    K_cpsLOC
221500061120     C                   KFLD                    K_cpsSIN
221600000104     C*
221700000104     C* AZZERO INIZIALE INDICATORI
221800061017     C                   SETOFF                                       40
221900000000     C*
222000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
222100000000     C     XMSGPA        PLIST
222200000000     C                   PARM                    DSMSMS
222300000000     C                   PARM                    DSMSPG
222400000000     C                   PARM                    DSMSKE
222500000000     C                   PARM                    DSMSER
222600000000     C*
222700000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
222800000000     C                   CALL      'XKEY'
222900000000     C                   PARM                    DSKEY
223000000000     C*
223100000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
223200000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
223300000000     C*
223400000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
223500000000     C*   -NOME PGM CHIAMANTE
223600000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
223700061017     C                   MOVEL     'TISP20R'     DSMSPG
223800061017     C                   MOVEL     'TISP20R'     MSGQUE
223900000000     C                   MOVEL     *BLANKS       MSGKEY
224000000000     C                   MOVEL     *BLANKS       DSMSMS
224100000000     C*
224200000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
224300000000     C                   Z-ADD     0             FUNGRU            5 0
224400000000     C*
224500000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
224600000000     C                   EXSR      FUNGES
224700000000     C*
224800000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
224900000000     C                   MOVEL     '0'           F12L00            1
225000000000     C                   MOVEL     '0'           F03L00            1
225100000000     C                   MOVEL     '0'           FINL00            1
225200000000     C                   MOVEL     '0'           ERRL00            1
225300000000     C                   MOVEL     '0'           FLFINE            1
225400000000     C*
225500000000     C* LANCIO ROUTINE PER VERIFICARE SE
225600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
225700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
225800020917     C                   EXSR      REPDATIUTE
225900000000     C*
226000000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
226100000000     C                   EXSR      AZZVID
226200000000     C*
226300000000     C                   ENDSR
226400000000     C*------------------------------------------------------------------------*
226500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
226600000000     C*------------------------------------------------------------------------*
226700000000     C     FUNGES        BEGSR
226800000000     C*
226900000000     C* PULISCO CAMPI DI LAVORO
227000000000     C                   Z-ADD     0             W                 5 0
227100000000     C*
227200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
227300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
227400000000     C     1             DO        24            I
227500000000     C                   MOVEL     FUD(I)        FUDRIG
227600000000     C*
227700000000     C* PULIZIA FUNZIONI PRECEDENTI
227800000000     C                   MOVEL     *BLANKS       FUV(I)
227900000000     C*
228000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
228100000000     C* (SALTA F24)
228200000000     C     FUDATT        IFEQ      'S'
228300000000     C     FUDFUN        ANDNE     '24'
228400000000     C                   ADD       1             W
228500000000     C                   MOVEL     FUDDES        FUV(W)
228600000000     C                   END
228700000000     C                   END                                                    DO WHILE
228800000000     C*
228900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
229000000000     C* ALTRIMENTI DISATTIVO F24
229100000000     C     1             DO        24            I
229200000000     C                   MOVEL     FUD(I)        FUDRIG
229300000000     C*
229400000000     C     FUDFUN        IFEQ      '24'
229500000000     C     W             IFGT      6
229600000000     C                   MOVEL     'S'           FUDATT
229700000000     C                   ELSE
229800000000     C                   MOVEL     *BLANKS       FUDATT
229900000000     C                   END                                                    W GT 8
230000000000     C                   MOVEL     FUDRIG        FUD(I)
230100000000     C                   END
230200000000     C                   END                                                    DO WHILE
230300000000     C*
230400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
230500000000     C     W             DIV       6             I
230600000000     C                   MVR                     Z
230700000000     C*
230800000000     C     Z             IFNE      0
230900000000     C     I             ADD       1             Z
231000000000     C                   ELSE
231100000000     C                   Z-ADD     I             Z                 5 0
231200000000     C                   END
231300000000     C*
231400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
231500000000     C                   ADD       1             FUNGRU
231600000000     C     FUNGRU        IFGT      Z
231700000000     C     FUNGRU        ORLT      1
231800000000     C                   Z-ADD     1             FUNGRU
231900000000     C                   END
232000000000     C*
232100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
232200000000     C     FUNGRU        IFEQ      1
232300000000     C                   MOVEL     FUV(1)        FUN001
232400000000     C                   MOVEL     FUV(2)        FUN002
232500000000     C                   MOVEL     FUV(3)        FUN003
232600000000     C                   MOVEL     FUV(4)        FUN004
232700000000     C                   MOVEL     FUV(5)        FUN005
232800000000     C                   MOVEL     FUV(6)        FUN006
232900000000     C                   END
233000000000     C*
233100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
233200000000     C     FUNGRU        IFEQ      2
233300000000     C                   MOVEL     FUV(7)        FUN001
233400000000     C                   MOVEL     FUV(8)        FUN002
233500000000     C                   MOVEL     FUV(9)        FUN003
233600000000     C                   MOVEL     FUV(10)       FUN004
233700000000     C                   MOVEL     FUV(11)       FUN005
233800000000     C                   MOVEL     FUV(12)       FUN006
233900000000     C                   END
234000000000     C*
234100000000     C* TERZO GRUPPO DI 6 FUNZIONI
234200000000     C     FUNGRU        IFEQ      3
234300000000     C                   MOVEL     FUV(13)       FUN001
234400000000     C                   MOVEL     FUV(14)       FUN002
234500000000     C                   MOVEL     FUV(15)       FUN003
234600000000     C                   MOVEL     FUV(16)       FUN004
234700000000     C                   MOVEL     FUV(17)       FUN005
234800000000     C                   MOVEL     FUV(18)       FUN006
234900000000     C                   END
235000000000     C*
235100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
235200000000     C     FUNGRU        IFEQ      4
235300000000     C                   MOVEL     FUV(19)       FUN001
235400000000     C                   MOVEL     FUV(20)       FUN002
235500000000     C                   MOVEL     FUV(21)       FUN003
235600000000     C                   MOVEL     FUV(22)       FUN004
235700000000     C                   MOVEL     *BLANKS       FUN005
235800000000     C                   MOVEL     *BLANKS       FUN006
235900000000     C                   END
236000000000     C*
236100000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
236200000000     C                   MOVEL     *BLANKS       FUN007
236300000000     C                   MOVEL     *BLANKS       FUN008
236400000000     C*
236500000000     C     1             DO        24            I
236600000000     C                   MOVEL     FUD(I)        FUDRIG
236700000000     C     FUDFUN        IFEQ      '24'
236800000000     C     FUDATT        ANDEQ     'S'
236900000000     C                   MOVEL     FUDDES        FUN008
237000000000     C                   END
237100000000     C                   END                                                    DO
237200000000     C                   ENDSR
237300000000     C*------------------------------------------------------------------------*
237400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
237500000000     C*------------------------------------------------------------------------*
237600000000     C     FUNCON        BEGSR
237700000000     C*
237800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
237900000000     C                   EXSR      FUNVAR
238000000000     C*
238100000000     C* PULISCO CAMPI DI LAVORO
238200000000     C                   Z-ADD     0             I                 5 0
238300000000     C                   Z-ADD     0             W                 5 0
238400000000     C*
238500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
238600000000     C     FUNKEY        IFNE      *BLANKS
238700000000     C     1             DO        24            I
238800000000     C                   MOVEL     FUD(I)        FUDRIG
238900000000     C*
239000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
239100000000     C     FUNKEY        IFEQ      FUDFUN
239200000000     C     FUDATT        ANDEQ     'S'
239300000000     C                   Z-ADD     I             W
239400000000     C                   END
239500000000     C                   END                                                    DO WHILE
239600000000     C*
239700000000     C* FUNZIONE NON ATTIVA (ERRORE)
239800000000     C     W             IFEQ      0
239900000000     C*
240000000000     C* EMISSIONE MESSAGGIO DI ERRORE
240100000000     C                   MOVEL     MSG(1)        DSMSMS
240200000000     C*
240300000000     C                   END                                                    W=0
240400000000     C                   END                                                    FUNKEY NE *BLANKS
240500000000     C*
240600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
240700000000     C     W             IFNE      0
240800000000     C     FUNKEY        OREQ      *BLANKS
240900000000     C*
241000000000     C     FUNKEY        IFEQ      *BLANKS
241100000000     C                   EXSR      ENTGES
241200000000     C                   END
241300000000     C     FUNKEY        IFEQ      '03'
241400000000     C                   EXSR      F03GES
241500000000     C                   END
241600000000     C     FUNKEY        IFEQ      '06'
241700000000     C                   EXSR      F06GES
241800000000     C                   END
241900000000     C     FUNKEY        IFEQ      '12'
242000000000     C                   EXSR      F12GES
242100000000     C                   END
242200000000     C     FUNKEY        IFEQ      '24'
242300000000     C                   EXSR      F24GES
242400000000     C                   END
242500000000     C     FUNKEY        IFEQ      'HP'
242600000000     C                   EXSR      HELGES
242700000000     C                   END
242800000000     C*
242900000000     C                   END
243000000000     C*
243100000000     C                   ENDSR
243200000000     C*------------------------------------------------------------------------*
243300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
243400000000     C*------------------------------------------------------------------------*
243500000000     C     FUNVAR        BEGSR
243600000000     C*
243700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
243800000000     C*
243900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
244000000000     C     KEY           IFEQ      ENTER
244100000000     C                   MOVEL     *BLANKS       FUNKEY
244200000000     C                   END
244300000000     C     KEY           IFEQ      F1
244400000000     C                   MOVEL     '01'          FUNKEY
244500000000     C                   END
244600000000     C     KEY           IFEQ      F2
244700000000     C                   MOVEL     '02'          FUNKEY
244800000000     C                   END
244900000000     C     KEY           IFEQ      F3
245000000000     C                   MOVEL     '03'          FUNKEY
245100000000     C                   END
245200000000     C     KEY           IFEQ      F4
245300000000     C                   MOVEL     '04'          FUNKEY
245400000000     C                   END
245500000000     C     KEY           IFEQ      F5
245600000000     C                   MOVEL     '05'          FUNKEY
245700000000     C                   END
245800991103     C     KEY           IFEQ      F6
245900000000     C                   MOVEL     '06'          FUNKEY
246000000000     C                   END
246100000000     C     KEY           IFEQ      F7
246200000000     C                   MOVEL     '07'          FUNKEY
246300000000     C                   END
246400000000     C     KEY           IFEQ      F8
246500000000     C                   MOVEL     '08'          FUNKEY
246600000000     C                   END
246700000000     C     KEY           IFEQ      F9
246800000000     C                   MOVEL     '09'          FUNKEY
246900000000     C                   END
247000000000     C     KEY           IFEQ      F10
247100000000     C                   MOVEL     '10'          FUNKEY
247200000000     C                   END
247300000000     C     KEY           IFEQ      F11
247400000000     C                   MOVEL     '11'          FUNKEY
247500000000     C                   END
247600000000     C     KEY           IFEQ      F12
247700000000     C                   MOVEL     '12'          FUNKEY
247800000000     C                   END
247900000000     C     KEY           IFEQ      F13
248000000000     C                   MOVEL     '13'          FUNKEY
248100000000     C                   END
248200000000     C     KEY           IFEQ      F14
248300000000     C                   MOVEL     '14'          FUNKEY
248400000000     C                   END
248500000000     C     KEY           IFEQ      F15
248600000000     C                   MOVEL     '15'          FUNKEY
248700000000     C                   END
248800000000     C     KEY           IFEQ      F16
248900000000     C                   MOVEL     '16'          FUNKEY
249000000000     C                   END
249100000000     C     KEY           IFEQ      F17
249200000000     C                   MOVEL     '17'          FUNKEY
249300000000     C                   END
249400000000     C     KEY           IFEQ      F18
249500000000     C                   MOVEL     '18'          FUNKEY
249600000000     C                   END
249700000000     C     KEY           IFEQ      F19
249800000000     C                   MOVEL     '19'          FUNKEY
249900000000     C                   END
250000000000     C     KEY           IFEQ      F20
250100000000     C                   MOVEL     '20'          FUNKEY
250200000000     C                   END
250300000000     C     KEY           IFEQ      F21
250400000000     C                   MOVEL     '21'          FUNKEY
250500000000     C                   END
250600000000     C     KEY           IFEQ      F22
250700000000     C                   MOVEL     '22'          FUNKEY
250800000000     C                   END
250900000000     C     KEY           IFEQ      F23
251000000000     C                   MOVEL     '23'          FUNKEY
251100000000     C                   END
251200000000     C     KEY           IFEQ      F24
251300000000     C                   MOVEL     '24'          FUNKEY
251400000000     C                   END
251500000000     C     KEY           IFEQ      ROLLUP
251600000000     C                   MOVEL     'RU'          FUNKEY
251700000000     C                   END
251800000000     C     KEY           IFEQ      HELP
251900000000     C                   MOVEL     'HP'          FUNKEY
252000000000     C                   END
252100000000     C                   ENDSR
252200000000     C*------------------------------------------------------------------------*
252300000000     C* NOTE PER COMPILAZIONE SCHIERE
252400000000     C*------------------------------------------------------------------------*
252500000000     C* -MSG-
252600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
252700000000     C* (NON TOCCARE MSG,1)
252800000000     C*
252900000000     C* -CMD-
253000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
253100000000     C*
253200000000     C* -FUD-
253300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
253400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
253500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
253600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
253700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
253800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
253900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
254000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
254100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
254200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
254300000000     C*
254400000000     C*------------------------------------------------------------------------*
254500101112     OPRTF198   E            rigaErr_T   1
254600101112     O                       wInt1
254700061017     OPRTF198   E            rigaErr     1
254800061017     O                       wRiga
254900061023     C*------------------------------------------------------------------------*
255000101112     OPRTF1982  E            rigaSI95_T  1
255100101112     O                       wInt2
255200061120     OPRTF1982  E            rigaSI95    1
255300061023     O                       wSI95
255400061120     C*------------------------------------------------------------------------*
255500101112     OQSYSPRT   E            rigaSin_T   1
255600101112     O                       wInt3
255700061120     OQSYSPRT   E            rigaSin     1
255800061122     O                       d_dup_cap           +1
255900061120     O                       key_sin_loc         +1
256000061122     O                       d_dup_sin           +1
256100061120     O                       wrk_error           +1
256200991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
256300061017TISP20R- Tasto funzionale non gestito.                                         1
256400061017TISP20R-                                                                       2
256500061017TISP20R-                                                                       3
256600061017TISP20R-                                                                       4
256700061017TISP20R- Errori nella funzione di richiesta versioni. Avvertire il CED         5
256800061017TISP20R-                                                                       6
256900061017TISP20R-                                                                       7
257000061017TISP20R- Versione Cappario BAR inesistente.                                    8
257100061017TISP20R-                                                                       9
257200061017TISP20R- Versione obbligatoria, inserire                                      10
257300061017TISP20R-                                                                      11
257400061017TISP20R-                                                                      12
257500061017TISP20R-                                                                      13
257600061017TISP20R-                                                                      14
257700061017TISP20R-                                                                      15
257800061017TISP20R-                                                                      16
257900061017TISP20R-                                                                      17
258000061017TISP20R-                                                                      18
258100061017TISP20R-                                                                      19
258200061017TISP20R-                                                                      20
258300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
258400000000                   -        (S/N)=N                        funz.01¬
258500000000                   -        (S/N)=N                        funz.02¬
258600000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
258700000000                   -        (S/N)=N                        funz.04¬
258800000000                   -        (S/N)=N                        funz.05¬
258900001120F06=Esecuzione     -        (S/N)=S                        funz.06¬
259000000104                   -        (S/N)=N                        funz.07¬
259100000104F08=Ins. Versione  -        (S/N)=N                        funz.08¬
259200000000                   -        (S/N)=N                        funz.09¬
259300000000                   -        (S/N)=N                        funz.10¬
259400000000                   -        (S/N)=N                        funz.11¬
259500991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
259600000000                   -        (S/N)=N                        funz.13¬
259700000000                   -        (S/N)=N                        funz.14¬
259800000000                   -        (S/N)=N                        funz.15¬
259900000000                   -        (S/N)=N                        funz.16¬
260000000000                   -        (S/N)=N                        funz.17¬
260100000000                   -        (S/N)=N                        funz.18¬
260200000000                   -        (S/N)=N                        funz.19¬
260300000000                   -        (S/N)=N                        funz.20¬
260400000000                   -        (S/N)=N                        funz.21¬
260500000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
260600000000                   -        (S/N)=N                        funz.23¬
260700991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
