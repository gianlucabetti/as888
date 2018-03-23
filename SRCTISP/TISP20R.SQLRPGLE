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
001600140611     Fazcos01l  UF A E           K DISK
001700061017     Fwfdcpl00f O  A E             DISK    prefix(diff_)
001800061017     FPRTF198   O    F  198        PRINTER
001900061023     FPRTF1982  O    F  198        PRINTER
002000061120     FQSYSPRT   O    F  132        PRINTER
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400000000     D*-------------------
002500000000     D* MESSAGGI
002600000000     D*-------------------
002700000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800000000     D*-------------------
002900000000     D* DESCRIZIONI TASTI FUNZIONALI
003000000000     D*-------------------
003100000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200000000     D*-------------------
003300000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003400000000     D*-------------------
003500000000     D FUV             S             19    DIM(24)
003600000417     D*-------------------
003700000000     D*------------------------------------------------------------------------*
003800000000     D* INPUT
003900000000     D*------------------------------------------------------------------------*
004000000000     D*-------------------
004100000000     D* RIDEFINIZIONE SCHIERA -FUD-
004200000000     D*-------------------
004300000000     D FUDDS           DS
004400000000     D*                -DESCRIZIONE PARZIALE
004500000000     D  FUDDE1                 1      1
004600000000     D*                -FUNZIONE
004700000000     D  FUDFUN                 2      3
004800000000     D*                -DESCRIZIONE PARZIALE
004900000000     D  FUDDE2                 4     19
005000000000     D*                -FILLER
005100000000     D  FUDFI0                20     34
005200000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005300000000     D  FUDATT                35     35
005400000000     D*                -FILLER
005500000000     D  FUDFI1                36     80
005600000000     D*                -DESCRIZIONE COMPLETA
005700000000     D  FUDDES                 1     19
005800000000     D*                -RIDEFINIZIONE RIGA
005900000000     D  FUDRIG                 1     80
006000000000     D*-------------------
006100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006200000000     D*-------------------
006300000000     D DSMSG         E DS
006400000000     D*-------------------
006500020917     D* DS REPERIMENTO DATI UTENTE
006600020917     D*------------
006700020917     D TIBS34DS      E DS                                                       *Profili utente
006800020917     D DDATIUTE      E DS                                                       *Dati utente
006900020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000061017     D*-------------------
007100061017     D* ARCHITETTURA
007200061017     D*-------------------
007300061017     D KPJBA         E DS
007400000000     D*-------------------
007500000000     D* DS PER GESTIONE TASTI FUNZIONALI
007600000000     D*-------------------
007700000000     D DSKEY         E DS
007800000000     D DEVDS1          DS
007900000000     D  KEY                  369    369
008000061017     D*------------------------------------------------------------------------*
008100061017     D* DS
008200061017     D*------------------------------------------------------------------------*
008300061017     D*
008400061017     D DSSI00        E DS                  extname(tisp00ds)                    *ds di procedura
008500061017     D*
008600061017     D ds_cpl_obs    E DS                  extname(azcpl00f) prefix(O_)
008700061017     D*
008800061017     D ds_cpc_k      E DS                  extname(azcpc00f) prefix(K_)
008900061017     D ds_cpl_k      E DS                  extname(azcpl00f) prefix(K_)
009000140611     D ds_cps_k      E DS                  extname(azcps00f) prefix(K_)
009100140611     D ds_cos_k      E DS                  extname(azcos00f) prefix(K_)
009200140611     D ds_cos_w      E DS                  extname(azcos00f) prefix(w_)
009300061017     D*
009400061017     D ds_cap_pt     E DS                  extname(cap_cap)  prefix(wc_)
009500061017     D ds_loc_pt     E DS                  extname(cap_loc)  prefix(wl_)
009600061017     D*
009700061017     D ds_cap_sin    E DS                  extname(cap_sin)  prefix(s_)
009800061017     D*
009900061017     D ds_loc_uniq     DS
010000061017     D   uniq_loc                    35
010100061017     D   uniq_prv                     2
010200061023     D   uniq_pra                     2
010300061017     D   uniq_cnt                     9  0
010400061017     D*
010500061017     D ds_loc_rainer   DS
010600061017     D   rainer_loc                  50
010700061017     D   rainer_cap                   9
010800061017     D   rainer_cnt                   9  0
010900061017     D*
011000061017     D ds_com_pt       DS
011100061017     D   com_com                     50
011200061017     D*
011300061017     D ds_loc          DS
011400061017     D   loc_ver                           like(cplver)
011500061017     D   loc_nar                           like(cplnar)
011600061017     D   loc_cap                           like(cplcap)
011700061017     D   loc_loc                           like(cplloc)
011800061024     D*
011900061024     D ds_prv          DS
012000061024     D   prv_prv                           like(cpcprv)
012100061024     D   prv_pra                           like(cpcpra)
012200061023     D*
012300061023     D tisi95ds      e ds
012400061120     D*
012500061120     D ds_cnt_sin      DS                  inz
012600061120     D   cnt_sin_cap                       like(cpscap)
012700061120     D   cnt_sin_sin                       like(cpssin)
012800061017     D*------------------------------------------------------------------------*
012900061017     D* VARIABILI D WRK
013000061017     D*------------------------------------------------------------------------*
013100061017     D wRiga           s            198    inz
013200061023     D wSI95           s            198    inz
013300061017     D wSQL            s            300    inz
013400101112     D wInt1           s            100    inz
013500101112     D wInt2           s            100    inz
013600101112     D wInt3           s            100    inz
013700061017     D wCPCVER         s                   inz like(cpcver)
013800061017     D wCPCCAP         s                   inz like(cpccap)
013900061017     D wCPLLOC         s                   inz like(cplloc)
014000061017     D wCPLESS         s                   inz like(cpless)
014100061023     D wCPLLIV         s                   inz like(cplliv)
014200061017     D wCPSCAP         s                   inz like(cpscap)
014300061017     D wCPSLOC         s                   inz like(cpsloc)
014400061017     D savcap          S                   inz like(cpccap)
014500061017     D chkobs          S              1    inz('1')
014600061122     D key_sin_loc     S                   inz like(cpsloc)
014700061122     D wrk_error       S              1    inz(*blanks)
014800061122     D w_cnt_sin       S              5  0 inz
014900061122     D*------------------------------------------------------------------------*
015000061122     D* DS E SCHIERA X ANALISI PREVENTIVA SITUAZIONE SINONIMI
015100061122     D*------------------------------------------------------------------------*
015200061122     D ds_dup_sin      DS                  inz
015300061122     D   d_dup_fi1                         inz like(cplfi1)
015400061122     D   d_dup_cap                         inz like(cpscap)
015500061122     D   d_dup_sin                         inz like(cpssin)
015600061122     D   d_dup_loc                         inz like(cpsloc)
015700000000     C*------------------------------------------------------------------------*
015800000000     C* MAIN LINE
015900000000     C*------------------------------------------------------------------------*
016000080317     C*
016100080317     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
016200080317     C
016300080317     C/EXEC SQL
016400080317     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016500080317     C/END-EXEC
016600000000     C*
016700000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
016800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
016900000000     C*
017000000000     C* CICLO FINO A CHE:
017100000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
017200000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
017300000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
017400000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
017500000000     C     F12L00        OREQ      '1'
017600000000     C     FLFINE        OREQ      '1'
017700000000     C*
017800000000     C* EMETTE FORMATO VIDEO
017900000000     C                   EXSR      BUTFMR
018000000000     C*
018100000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
018200000000     C                   EXSR      FUNCON
018300000000     C*
018400000000     C                   END                                                    <-- 2 --
018500000000     C*
018600000000     C                   END                                                    <-- 1 --
018700000000     C*
018800000000     C                   SETON                                        LR
018900000000     C*------------------------------------------------------------------------*
019000000000     C* EMISSIONE FORMATO A VIDEO
019100000000     C*------------------------------------------------------------------------*
019200000000     C     BUTFMR        BEGSR
019300000000     C*
019400000000     C* WRITE RECORD TESTATA
019500061017     C                   WRITE     SP20D01
019600000000     C*
019700000000     C* VISUALIZZAZIONE MESSAGGIO
019800000000     C                   EXSR      SNDMSG
019900000000     C*
020000000000     C* WRITE RECORD FUNZIONI
020100061017     C                   WRITE     SP20D02
020200991019     C*
020300000417     C* EXFMT RECORD DETTAGLIO E DECODIFICA
020400000417     C                   EXSR      DECODI
020500061017     C                   EXFMT     SP20D03
020600991019     C*
020700000000     C* AZZERO FLAG DI ERRORE LIVELLO
020800000000     C                   MOVEL     '0'           ERRL00
020900000000     C*
021000000000     C* AZZERO FLAG DI ERRORE
021100061017     C                   SETOFF                                       40
021200000000     C*
021300000000     C                   ENDSR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* SNDMSG - MANDA MESSAGGIO
021600000000     C*------------------------------------------------------------------------*
021700000000     C     SNDMSG        BEGSR
021800000000     C*
021900000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
022000061017     C                   MOVEL     '1'           *IN43
022100061017     C                   WRITE     SP20DMC
022200061017     C                   MOVEL     '0'           *IN43
022300000000     C*
022400000000     C* VISUALIZZAZIONE MESSAGGIO
022500000000     C                   Z-ADD     0             MSGULT            4 0
022600000000     C                   CALL      'XMSG'        XMSGPA
022700000000     C     DSMSER        IFEQ      0
022800000000     C                   MOVEL     DSMSKE        MSGKEY
022900000000     C                   END
023000000000     C                   Z-ADD     1             MSGULT
023100061017     C                   WRITE     SP20DMS                                      SFL MESS
023200061017     C                   WRITE     SP20DMC                                      CONTR MESS
023300000000     C*
023400000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
023500000000     C                   MOVEL     *BLANKS       DSMSMS
023600000000     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
024000000000     C*------------------------------------------------------------------------*
024100000000     C     ENTGES        BEGSR
024200000000     C*
024300000000     C* CONTROLLA I CAMPI IMMESSI
024400000000     C                   EXSR      DECODI
024500000000     C*
024600000000     C                   ENDSR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
024900000000     C*------------------------------------------------------------------------*
025000000000     C     F03GES        BEGSR
025100000000     C*
025200000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
025300000000     C                   MOVEL     '1'           F03L00
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700061017     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
025800000000     C*------------------------------------------------------------------------*
025900991020     C     F06GES        BEGSR
026000991105     C*
026100991105     C                   EXSR      DECODI
026200991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
026300991105     C     ERRL00        IFEQ      '0'
026400101112     C*
026500101112     C* Stampo le intestazioni delle stampe
026600101115     C                   eval      wInt1 = 'SEGNALAZ. CAP + ANOMALIE LOCALITA'
026700101112     C                   except    rigaErr_T
026800101112     C*
026900101115     C                   eval      wInt2 = 'VERIFICA LOCALITA AMBIGUE ' +
027000101115     C                                     '(secondo TISI95)'
027100101112     C                   except    rigaSI95_T
027200101112     C*
027300101115     C                   eval      wInt3 = 'ANNULLAMENTI/ERRORI SINONIMI'
027400101112     C                   except    rigaSin_T
027500991105     C*
027600000414     C* LANCIO LA ROUTINE DI RIEMPIMENTO ARCHIVI CAPPARIO POSTE DA BARTOLINI
027700061017     C                   EXSR      ELABORA
027800061023     C*
027900061023     C* Effettuo lancio TISI95 solo x chiusura
028000061023     C                   CLEAR                   TISI95DS
028100061023     C                   EVAL      I95TLA = 'C'
028200061023     C                   CALL      'TISI95R'
028300061023     C                   PARM                    TISI95DS
028400061023     C*
028500000605     C                   ENDIF
028600000605     C*
028700000000     C                   ENDSR
028800000000     C*------------------------------------------------------------------------*
028900000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
029000000000     C*------------------------------------------------------------------------*
029100000000     C     F12GES        BEGSR
029200000000     C*
029300000000     C* IMPOSTO FLAG DI LIVELLO
029400991103     C                   MOVEL     '1'           F12L00
029500000000     C*
029600000000     C                   ENDSR
029700000000     C*------------------------------------------------------------------------*
029800991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
029900000000     C*------------------------------------------------------------------------*
030000000000     C     F24GES        BEGSR
030100000000     C*
030200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
030300000000     C                   EXSR      FUNGES
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
030800000000     C*------------------------------------------------------------------------*
030900000000     C     HELGES        BEGSR
031000000000     C                   ENDSR
031100991020     C*------------------------------------------------------------------------*
031200061017     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
031300000000     C*------------------------------------------------------------------------*
031400000000     C     DECODI        BEGSR
031500000104     C*
031600000413     C* CONTROLLO VERSIONE CAPPARIO  BARTOLINI
031700000104     C     ERRL00        IFEQ      '0'
031800000104     C                   EXSR      CHKVER
031900000413     C                   ENDIF
032000000104     C*
032100000000     C                   ENDSR
032200000414     C*------------------------------------------------------------------------*
032300061017     C*  CHKVER - CONTROLLA PRESENZA O MENO DELLA VERSIONE BARTOLINI
032400000104     C*------------------------------------------------------------------------*
032500000104     C     CHKVER        BEGSR
032600000413     C*
032700000413     C                   IF        %trim(VIDVER)='?'
032800000413     C*
032900000413     C* lancia il programma di gestione versioni BAR in ricerca
033000000414     C                   CLEAR                   dssi00                         *pulisce ds
033100000414     C                   MOVEL     'R01'         d00op0                         *livello procedura
033200000414     C                   MOVEL     *blanks       d00op1                         *livello programma
033300000414     C                   MOVEL     '0'           d00f03                         *premuto F03
033400000414     C                   MOVEL     '0'           d00f12                         *premuto F12
033500000414     C                   MOVEL     '0'           d00err                         *errore
033600000414     C                   MOVEL     *blanks       d00msg                         *messaggio ritorno
033700000414     C                   CALL      'TISI46R'                                    *gestione versioni
033800000414     C                   PARM                    kpjba
033900000414     C                   PARM                    dssi00
034000000413     C* ritorno
034100000414     C                   IF        d00err='1'
034200000413     C                   SETON                                        81
034300000413     C                   MOVEL     MSG(5)        DSMSMS
034400000414     C                   ELSE
034500000414     C                   MOVE(p)   d00ver        VIDVER
034600061017     C                   Z-ADD     d00ver        VIHVER            5 0
034700000414     C                   ENDIF
034800000413     C*
034900000413     C                   ELSE
035000000414     C                   call(e)   'ISNUMERIC'
035100000414     C                   PARM      VIDVER        PiStr            30
035200000414     C                   PARM      ','           PiDecChr          1
035300000414     C                   PARM      *ZEROS        PiVal            30 9
035400000414     C                   PARM      '0'           PiInt             1
035500000414     C                   PARM      '0'           PiNum             1
035600061017     C                   IF        NOT %error AND
035700000414     C                             PiInt=*ON
035800000414     C                   Z-ADD     PiVal         VIHVER
035900061017     C     VIHVER        CHAIN     azcpr01l
036000061017     C                   IF        not %found(azcpr01l)
036100061017     C                   SETON                                        40
036200061017     C                   MOVEL     MSG(8)        DSMSMS
036300061017     C                   MOVEL     '1'           ERRL00
036400061023     C                   ELSE
036500061023     C                   Z-ADD     CPRDDE        datcor            8 0
036600061017     C                   ENDIF
036700000417     C                   ELSE
036800061017     C                   MOVEL     '1'           ERRL00
036900000414     C                   ENDIF
037000000413     C                   ENDIF
037100061017     C*
037200061017     C                   IF        ERRL00 <> '1'
037300061017     C                   Z-ADD     VIHVER        NumVER            5 0
037400061017     C                   Z-ADD     *zeros        NumPrecVER        5 0
037500061017     C                   EVAL      NumPrecVER = NumVER - 1
037600061017     C                   ENDIF
037700000105     C*
037800000104     C                   ENDSR
037900000000     C*------------------------------------------------------------------------*
038000061017     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
038100000000     C*------------------------------------------------------------------------*
038200000000     C     AZZVID        BEGSR
038300061018     C*
038400061018     C                   SETOFF                                       919293
038500061018     C                   SETOFF                                       949596
038600061120     C                   SETOFF                                       9798
038700000000     C*
038800061017     C                   MOVEL     *BLANKS       VIDINZ
038900061017     C                   MOVEL     *BLANKS       VIDIMP
039000061017     C                   MOVEL     *BLANKS       VIDCNOP
039100061017     C                   MOVEL     *BLANKS       VIDLNOP
039200061017     C                   MOVEL     *BLANKS       VIDANNS
039300061120     C                   MOVEL     *BLANKS       VIDANNS2
039400061017     C                   MOVEL     *BLANKS       VIDOBSC
039500061023     C                   MOVEL     *BLANKS       VIDSI95
039600061018     C                   MOVEL     *BLANKS       VID01
039700061018     C                   MOVEL     *BLANKS       VID02
039800061018     C                   MOVEL     *BLANKS       VID03
039900061018     C                   MOVEL     *BLANKS       VID04
040000061018     C                   MOVEL     *BLANKS       VID05
040100061018     C                   MOVEL     *BLANKS       VID06
040200061018     C                   MOVEL     *BLANKS       VID07
040300061120     C                   MOVEL     *BLANKS       VID08
040400061017     C*
040500000104     C                   ENDSR
040600000414     C*------------------------------------------------------------------------*
040700061017     C* ELABORA - ROUTINE DI GENERAZIONE ARCHIVI CAPPARIO POSTE DA BARTOLINI
040800000414     C*------------------------------------------------------------------------*
040900061017     C     ELABORA       BEGSR
041000061018     C*
041100061120     C                   IF        VIDINZ  ='X'
041200061120     C                   EVAL      VID01   = 'Da elaborare'
041300061018     C                   ENDIF
041400061120     C                   IF        VIDIMP  ='X'
041500061120     C                   EVAL      VID02   = 'Da elaborare'
041600061018     C                   ENDIF
041700061120     C                   IF        VIDCNOP ='X'
041800061120     C                   EVAL      VID03   = 'Da elaborare'
041900061018     C                   ENDIF
042000061120     C                   IF        VIDLNOP ='X'
042100061120     C                   EVAL      VID04   = 'Da elaborare'
042200061018     C                   ENDIF
042300061120     C                   IF        VIDSI95 <>*blanks
042400061120     C                   EVAL      VID05   = 'Da elaborare'
042500061023     C                   ENDIF
042600061120     C                   IF        VIDOBSC ='X'
042700061120     C                   EVAL      VID06   = 'Da elaborare'
042800061018     C                   ENDIF
042900061120     C                   IF        VIDANNS ='X'
043000061120     C                   EVAL      VID07   = 'Da elaborare'
043100061023     C                   ENDIF
043200061120     C                   IF        VIDANNS2='X'
043300061120     C                   EVAL      VID08   = 'Da elaborare'
043400061120     C                   ENDIF
043500061018     C                   WRITE     SP20D03
043600000606     C*
043700061017     C* => Inizializza versioe da file d wrk
043800061017     C                   IF        VIDINZ='X'
043900061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
044000061018     C                   EXSR      SNDMSG
044100061017     C                   exsr      inzpt
044200000606     C                   ENDIF
044300000606     C*
044400061017     C* => Importa cappario Poste
044500061017     C                   IF        VIDIMP='X'
044600061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
044700061018     C                   EXSR      SNDMSG
044800061024     C                   exsr      updFCPU
044900061017     C                   exsr      execap                                       Elaborazione cap PT
045000061017     C                   exsr      exeloc                                       Elaborazione loc PT
045100000606     C                   ENDIF
045200000605     C*
045300061017     C* => Obsoleta CAP non in CAP Poste
045400061017     C                   IF        VIDCNOP='X'
045500061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
045600061018     C                   EXSR      SNDMSG
045700061017     C                   EXSR      exever1
045800000605     C                   ENDIF
045900061017     C*
046000061017     C* => Obsoleta LOC non in LOC Poste
046100061017     C                   IF        VIDLNOP='X'
046200061018     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
046300061018     C                   EXSR      SNDMSG
046400061017     C                   EXSR      exever2
046500061017     C                   ENDIF
046600061023     C*
046700061023     C* => Verifica località "?" tramite TISI95R
046800061023     C                   IF        VIDSI95<>*blanks
046900061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
047000061023     C                   EXSR      SNDMSG
047100061023     C                   EXSR      exever5
047200061023     C                   ENDIF
047300061023     C*
047400061023     C* => Obsoleta CAP con tutte LOC obsolete
047500061023     C                   IF        VIDOBSC='X'
047600061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
047700061023     C                   EXSR      SNDMSG
047800061023     C                   EXSR      exever4
047900061023     C                   ENDIF
048000061023     C*
048100061023     C* => Annulla SIN già LOC (x stesso CAP)
048200061023     C                   IF        VIDANNS='X'
048300061023     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
048400061023     C                   EXSR      SNDMSG
048500061023     C                   EXSR      exever3
048600061123     C*******
048700061123     C* Lasciare exever6 asteriscato! Usato solo la 1° volta x correggere 1 errore.
048800061123     C* Ora correzzione è già nel giro standard
048900061123     C***                EXSR      exever6    (OK asteriscato - usato solo 1° volta - )
049000061123     C*******
049100061023     C                   ENDIF
049200061120     C*
049300061120     C* => Annulla SIN già SIN (x stesso CAP)
049400061120     C                   IF        VIDANNS2='X'
049500061120     C                   EVAL      DSMSMS = 'Elaborazione in corso....'
049600061120     C                   EXSR      SNDMSG
049700061122     C                   EXSR      exever8
049800061120     C                   ENDIF
049900000414     C*
050000000414     C                   ENDSR
050100020917     C*--------------------------------------------------------------------------------------------*
050200020917     C* REPERISCE I DATI UTENTE
050300020917     C*--------------------------------------------------------------------------------------------*
050400020917     C     REPDATIUTE    BEGSR
050500020917     C*
050600020917     C* INIZIALIZZA VARIABILI DI WRK
050700020917     C                   CLEAR                   TIBS34DS
050800020917     C                   CLEAR                   AZUTEDS
050900020917     C                   CLEAR                   DDATIUTE
051000020917     C*
051100020917     C     *DTAARA       DEFINE    §azute        azuteds
051200020917     C     *DTAARA       DEFINE    §datiute      ddatiute
051300020917     C                   IN(E)     *DTAARA
051400061017     C                   IF        %Error
051500020917     c                   EVAL      I34Tla = 'L'
051600020917     C                   CALL      'TIBS34R'
051700020917     C                   PARM                    Tibs34Ds
051800020917     C                   IN        *DTAARA
051900061017     C                   ENDIF
052000020917     C*
052100020917     C                   ENDSR
052200061017     C*--------------------------------------------------------------------------------------------*
052300061017     C* execap - elaborazione cap
052400061017     C*--------------------------------------------------------------------------------------------*
052500061017     C     execap        BEGSR
052600101101     C*
052700101101     C* ======================================================> nuovo CAP 2010
052800101101     C*
052900101101     C* Scorro tutto il file variazioni cap poste x gestione modifiche cap / inseriimenti / cancellazioni
053000101101     C     *start        setll     lnk_cap
053100101101     C                   read      lnk_cap
053200101101     C                   dow       not %eof(lnk_cap)
053300101101     C*
053400101101     C                   select
053500101101     C*
053600101101     C* Se NUOVO CAP => Listo azione
053700101116     C                   when      OLDC_PRV= *blanks AND OLDC_CAP= *blanks AND
053800101116     C                             NEWC_PRV<>*blanks AND NEWC_CAP<>*blanks
053900140611     C*
054000140611     C* Verifico eventuale esistenza in cappario BRT
054100140611     C                   eval      K_cpcVER = NumVER
054200140611     C                   eval      K_cpcNAR = *blanks
054300140611     C                   eval      K_cpcCAP = NEWC_CAP
054400140611     C     KEYcpc01_C    chain     azcpc01l
054500140611     C                   if        not %found(azcpc01l)
054600101101     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
054700140611     C                             ' => CAP completamente NUOVO'+
054800140611     C                             '  ( gestire MANUALMENTE )'
054900140611     C                   else
055000140611     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
055100140611     C                             ' => CAP completamente NUOVO '+
055200140611     C                             'per PT - presente in cappario BRT ' +
055300140611     C                             '('+ cpcPRV +')' +
055400140611     C                             '  ( verificare e gestire MANUALMENTE )'
055500140611     C                   endif
055600140612     C                   except    rigaErr
055700101101     C*
055800101101     C* Se ELIM. CAP => Listo azione
055900101116     C                   when      OLDC_PRV<>*blanks AND OLDC_CAP<>*blanks AND
056000101116     C                             NEWC_PRV= *blanks AND NEWC_CAP= *blanks
056100101101     C                   eval      wRiga = OLDC_PRV + ' / ' + OLDC_CAP +
056200101102     C                             ' => presenti elim. totali o parziali  '+
056300101102     C                             '( verificare e gestire MANUALMENTE )'
056400101101     C                   except    rigaErr
056500101101     C*
056600101101     C* Se MODIFICA CAP => gestisco
056700101116     C                   when      OLDC_PRV<>*blanks AND OLDC_CAP<>*blanks AND
056800101116     C                             NEWC_PRV<>*blanks AND NEWC_CAP<>*blanks AND
056900101101     C                             (OLDC_PRV<>NEWC_PRV OR OLDC_CAP<>NEWC_CAP)
057000101101     C*
057100101101     C* Creo il nuovo cap duplicandolo dal suo precedente
057200101101     C                   eval      K_cpcVER = NumVER
057300101101     C                   eval      K_cpcNAR = *blanks
057400101101     C                   eval      K_cpcCAP = OLDC_CAP
057500101101     C     KEYcpc01_C    chain     azcpc01l
057600101101     C                   if        %found(azcpc01l)
057700101101     C*
057800101101     C* Verifico congruenza precisa provincie
057900101101     C                   if        OLDC_PRV <> cpcPRV AND OLDC_PRV <> cpcPRA
058000101101     C*
058100101101     C* Listo eventuale incongruenza
058200101101     C                   eval      wRiga = OLDC_PRV + ' / ' + OLDC_CAP +
058300110511     C                             ' => incongruenza dati BRT: '       +
058400101102     C                             cpcPRV + ' / ' + cpcCAP +
058500101102     C                             '  ( verificare MANUALMENTE )'
058600101101     C                   except    rigaErr
058700101101     C*
058800101101     C* Se tutto coincide => procedo
058900101101     C                   else
059000101101     C*
059100101101     C                   eval      cpcCAP = NEWC_CAP
059200101101     C                   eval      cpcPRV = NEWC_PRV
059300101101     C                   eval      cpcDTR = *all'7'
059400101101     C*
059500101101     C* Quindi inserisco il nuovo CAP
059600140612     C                   write(e)  azcpc000
059700101116     C*
059800101116     C* Listo azione
059900101116     C                   if        %error
060000101116     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
060100101116     C                             ' => fallita duplica da CAP: ' +
060200101116     C                             OLDC_PRV + ' / ' + OLDC_CAP
060300101116     C                   else
060400140612     C*
060500140612     C                   eval      K_cosVER = cpcVER
060600140612     C                   eval      K_cosNAR = cpcNAR
060700140612     C                   eval      K_cosCAP = cpcCAP
060800140612     C                   eval      K_cosLOC = *blanks
060900140612     C                   eval      w_cosCAP = wCPSCAP
061000140612     C                   eval      w_cosLOC = wCPSLOC
061100140612     C                   exsr      dupcos
061200140612     C*
061300101101     C                   eval      wRiga = NEWC_PRV + ' / ' + NEWC_CAP +
061400101101     C                             ' => duplicato da CAP: ' +
061500101101     C                             OLDC_PRV + ' / ' + OLDC_CAP
061600101116     C                   endif
061700140612     C                   except    rigaErr
061800101101     C                   endif
061900101101     C                   endif
062000101101     C*
062100101101     C                   endsl
062200101101     C*
062300101101     C* Proseguo con la lettura
062400101101     C                   read      lnk_cap
062500101101     C                   enddo
062600101115     C*
062700101115     C* Scorro tutto il file variazioni loc poste x gestione modifiche loc / inseriimenti / cancellazioni
062800101115     C     *start        setll     lnk_loc
062900101115     C                   read      lnk_loc
063000101115     C                   dow       not %eof(lnk_loc)
063100101115     C*
063200101115     C                   select
063300101115     C*
063400101115     C* Se NUOVA LOC => Listo azione
063500101115     C                   when       OLDL_PRV =  *blanks   AND
063600101115     C                              OLDL_CAP =  *blanks   AND
063700101115     C                              OLDL_LOC =  *blanks   AND
063800101115     C                             (NEWL_PRV <> *blanks   OR
063900101115     C                              NEWL_CAP <> *blanks   OR
064000101115     C                              NEWL_LOC <> *blanks)
064100140611     C*
064200140611     C* Verifico eventuale esistenza in cappario BRT
064300140611     C                   eval      K_cplVER = NumVER
064400140611     C                   eval      K_cplNAR = *blanks
064500140611     C                   eval      K_cplCAP = NEWL_CAP
064600140611     C                   eval      K_cplLOC = NEWL_LOC
064700140611     C     KEYcpl01_C    chain     azcpl01l
064800140611     C                   if        not %found(azcpl01l)
064900140611     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
065000140611     C                             ' => LOC completamente NUOVA  '+
065100140611     C                             '( gestire MANUALMENTE )'
065200140611     C                   else
065300140611     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
065400140611     C                             ' => LOC completamente NUOVA  '+
065500140611     C                             'per PT - presente in cappario BRT' +
065600140611     C                             '  ( verificare e gestire MANUALMENTE )'
065700140611     C                   endif
065800101115     C                   except    rigaErr
065900101115     C*
066000101115     C* Se ELIM. LOC => Listo azione
066100101115     C                   when      (OLDL_PRV <> *blanks   OR
066200101115     C                              OLDL_CAP <> *blanks   OR
066300101115     C                              OLDL_LOC <> *blanks)  AND
066400101115     C                              NEWL_PRV =  *blanks   AND
066500101115     C                              NEWL_CAP =  *blanks   AND
066600101115     C                              NEWL_LOC =  *blanks
066700101115     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
066800101115     C                             ' => presenti elim. totali o parziali  '+
066900101115     C                             '( verificare e gestire MANUALMENTE )'
067000101115     C                   except    rigaErr
067100101115     C*
067200101115     C* Se MODIFICA LOC => gestisco
067300101115     C                   when      (OLDL_PRV <> *blanks   OR
067400101115     C                              OLDL_CAP <> *blanks   OR
067500101115     C                              OLDL_LOC <> *blanks)  AND
067600101115     C                             (NEWL_PRV <> *blanks   OR
067700101115     C                              NEWL_CAP <> *blanks   OR
067800101115     C                              NEWL_LOC <> *blanks)  AND
067900101115     C                             (NEWL_PRV <> OLDL_PRV  OR
068000101115     C                              NEWL_CAP <> OLDL_CAP  OR
068100101116     C                              NEWL_LOC <> OLDL_LOC) AND
068200101116     C                              OLDL_CAP <> *blanks   AND
068300101116     C                              OLDL_LOC <> *blanks   AND
068400101116     C                              NEWL_CAP <> *blanks   AND
068500101116     C                              NEWL_LOC <> *blanks
068600101115     C*
068700101115     C* Creo la nuova località duplicandola dalla sua precedente
068800101115     C                   eval      K_cplVER = NumVER
068900101115     C                   eval      K_cplNAR = *blanks
069000101115     C                   eval      K_cplCAP = OLDL_CAP
069100101115     C                   eval      K_cplLOC = OLDL_LOC
069200101115     C     KEYcpl01_C    chain     azcpl01l
069300101115     C                   if        %found(azcpl01l)
069400101116     C*
069500101116     C* Innanzitutto obsoletizzo la "vecchia" località
069600101116     C                   movel     'O'           cplFI1
069700101116     C                   movel     *blanks       cplFTR
069800101116     C                   eval      cplDTR = *all'8'
069900101116     C                   update(e) azcpl000
070000140611     C*
070100140611     C                   if        not %error
070200140611     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
070300140611     C                                   ' => Obsoletizzata vecchia località'
070400140611     C                   except    rigaErr
070500140611     C                   endif
070600101116     C*
070700101116     C* Quindi inserisco la nuova località
070800101116     C                   movel     *blanks       cplFI1
070900101115     C                   eval      cplCAP = NEWL_CAP
071000101115     C                   eval      cplLOC = NEWL_LOC
071100101115     C                   eval      cplDTR = *all'7'
071200101115     C                   write(e)  azcpl000
071300140611     C*
071400140611     C                   if        not %error
071500140611     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
071600140611     C                                   ' => Inserita nuova località'
071700140611     C                   except    rigaErr
071800140611     C                   endif
071900101115     C*
072000101115     C* Listo azione
072100101115     C                   if        %error
072200101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
072300101115     C                                   ' => errore in inserimento nuova LOC'
072400101115     C                   else
072500101115     C*
072600101115     C* Se tutto ok => porto i sinonimi della "vecchia" località sulla nuova località
072700101115     C                   eval      K_cpsVER = NumVER
072800101115     C                   eval      K_cpsNAR = *blanks
072900101115     C                   eval      K_cpsCAP = OLDL_CAP
073000101115     C                   eval      K_cpsLOC = OLDL_LOC
073100101115     C                   eval      wCPSCAP  = NEWL_CAP
073200101115     C                   eval      wCPSLOC  = NEWL_LOC
073300101115     C***                exsr      movsin
073400101115     C                   exsr      dupsin
073500140611     C*
073600140611     C                   eval      K_cosVER = K_cpsVER
073700140611     C                   eval      K_cosNAR = K_cpsNAR
073800140611     C                   eval      K_cosCAP = K_cpsCAP
073900140611     C                   eval      K_cosLOC = K_cpsLOC
074000140611     C                   eval      w_cosCAP = wCPSCAP
074100140611     C                   eval      w_cosLOC = wCPSLOC
074200140611     C                   exsr      dupcos
074300101115     C*
074400101115     C                   eval      wRiga=NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC+
074500101115     C                             ' => duplicata da LOC: ' +
074600101115     C                             OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC
074700101115     C                   endif
074800140612     C                   except    rigaErr
074900101115     C*
075000101115     C* Se vecchia località NON trovata => listo anomalia
075100101115     C                   else
075200101115     C                   eval      wRiga=OLDL_PRV+' / '+OLDL_CAP+' / '+OLDL_LOC+
075300101115     C                                   ' => vecchia LOC non trovata, ' +
075400101115     C                                   'impossibile inserire nuova LOC: '+
075500101115     C                                   NEWL_PRV+' / '+NEWL_CAP+' / '+NEWL_LOC
075600101115     C                   except    rigaErr
075700101115     C                   endif
075800101115     C*
075900101115     C                   endsl
076000101115     C*
076100101115     C* Proseguo con la lettura
076200101115     C                   read      lnk_loc
076300101115     C                   enddo
076400101101     C*
076500101101     C* <======================================================
076600061017     C*
076700061017     C* Scorro tutto il file cap poste
076800061017     C     *start        setll     cap_cap
076900061017     C                   read      cap_cap
077000061017     C                   dow       not %eof(cap_cap)
077100061017     C*
077200061017     C                   setoff                                       50
077300061017     C*
077400061017     C* Verifico se trattasi d cap già esistente
077500061017     C                   eval      K_cpcVER = NumVER
077600061017     C                   eval      K_cpcNAR = *blanks
077700061017     C                   eval      K_cpcCAP = c_cap
077800061017     C     KEYcpc01_C    chain     azcpc01l
077900061017     C                   if        %found(azcpc01l)
078000061024     C*
078100061024     C* Se provincia altrenativa già valorizzata e provincia cap PT corrente diversa da entrambe (prv e pra) => listo
078200061024     C                   if        c_prv  <> cpcPRV  AND
078300061024     C                             c_prv  <> cpcPRA  AND
078400061024     C                             c_prv  <> *blanks AND
078500061024     C                             cpcPRV <> *blanks AND
078600061024     C                             cpcPRA <> *blanks
078700101101     C                   eval      wRiga = cpcCAP + '  '    +
078800061024     C                             '=> tentativo scrittura 3 provincie ' +
078900061024     C                             '('+cpcPRV+' '+cpcPRA+' '+c_prv+')'
079000061024     C                   except    rigaErr
079100061024     C                   else
079200061017     C*
079300061017     C* Se cap già esistente verifico se è rimasto sulla stessa provincia
079400061017     C                   if        c_prv = *blanks OR
079500061017     C                             c_prv = cpcPRV  OR
079600061017     C                             c_prv = cpcPRA
079700061017     C                   seton                                        50
079800061017     C                   else
079900061017     C*
080000061017     C* Se provincia altrenativa già valorizzata => listo il cap x successiva verifica manuale
080100061024     C                   if        cpcPRA <> *blanks and cpcFTR <> *blanks
080200061017     C                   eval      wRiga = cpcCAP + '  '    +
080300061017     C                             '=> provincia alternativa sovrascritta ' +
080400061017     C                             '(' + cpcPRA + ')'
080500061017     C                   except    rigaErr
080600061017     C                   endif
080700061017     C*
080800061017     C* Quindi imposto la "vecchia" provincia come provincia alternativa ed imposto come provincia
080900061017     C* quella "nuova" PT
081000061017     C                   eval      cpcPRA = cpcPRV
081100061017     C                   eval      cpcPRV = c_prv
081200061017     C                   eval      cpcFTR = *blanks
081300080326     C                   eval      cpcDTR = *all'8'
081400061017     C                   update(e) azcpc000
081500061017     C                   seton                                        50
081600061017     C                   endif
081700061024     C*
081800061024     C                   endif
081900061017     C*
082000061017     C                   else
082100061017     C*
082200061017     C* Se cap inesistente flaggo apposito campo FLG2 su file cap PT
082300061017     C                   eval      c_FLG2 = 'N'
082400061017     C                   update    cap000
082500061017     C                   eval      wRiga = c_prv + '  '    +
082600061017     C                                     c_cap + '  '    +
082700061017     C                             '=> CAP inesistente!!!'
082800061017     C                   except    rigaErr
082900061017     C                   endif
083000061017     C*
083100061017     C* Se cap PT è stato in qualche modo trattato flaggo record su file PT
083200061017     C                   if        *in50 = *on
083300061017     C                   eval      c_FLG1 = 'S'
083400061017     C                   update(e) cap000
083500061017     C                   endif
083600061017     C*
083700061017     C* Proseguo con la lettura dei cap PT
083800061017     C                   read      cap_cap
083900061017     C                   enddo
084000061017     C*
084100061017     C                   ENDSR
084200061017     C*--------------------------------------------------------------------------------------------*
084300061017     C* exeloc - elaborazione località
084400061017     C*--------------------------------------------------------------------------------------------*
084500061017     C     exeloc        BEGSR
084600061017     C*
084700061017     C* "Normalizzo" le sintassi delle località BART
084800061017     C                   exsr      normloc
084900061017     C*
085000061017     C* Scorro tutto il file località poste
085100061017     C     *start        setll     cap_loc
085200061017     C                   read      cap_loc
085300061017     C                   dow       not %eof(cap_loc)
085400061017     C*
085500061017     C                   setoff                                       51
085600061017     C                   clear                   wfdcpl00
085700061023     C                   clear                   wCPLLIV
085800061017     C*
085900061017     C* 1° => se cap+loc PT trovati esattamente in file località BART (azcpl)
086000061017     C                   eval      K_cplVER = NumVER
086100061017     C                   eval      K_cplNAR = *blanks
086200061017     C                   eval      K_cplCAP = l_cap
086300061017     C                   eval      K_cplLOC = l_loc
086400061017     C     KEYcpl01_C    chain     azcpl01l
086500061017     C                   if        %found(azcpl01l)
086600080404     C                   if        cplFI1 <> *blanks OR
086700080404     C                             cplATB <> *blanks
086800080404     C                   eval      cplFI1 = *blanks
086900080404     C                   eval      cplATB = *blanks
087000080404     C                   eval      cplDTR = *all'6'
087100080404     C                   update    azcpl000
087200080404     C                   endif
087300080404     C*
087400061017     C* Se trovata esattamente => eseguo gestione sinonimi
087500061017     C                   exsr      exesin
087600061017     C                   seton                                        51
087700061023     C                   else
087800061023     C*
087900061023     C* Reperisco la provincia dal cap BART
088000061023     C                   eval      K_cpcVER = NumVER
088100061023     C                   eval      K_cpcNAR = *blanks
088200061023     C                   eval      K_cpcCAP = l_cap
088300061023     C     KEYcpc01_C    chain     azcpc01l
088400061023     C                   if        %found(azcpc01l)
088500061017     C*
088600061017     C* 2° => se cap+loc PT nn trovati esattamente in file località BART (azcpl)
088700061017     C*       verifico se la località è univoca nella provincia
088800061017     C                   exsr      rtvlocuniq
088900061017     C*
089000061023     C* Se località univoca nella provincia procedo con elaborazione particolare
089100061017     C                   if        uniq_cnt = 1
089200061017     C*
089300061017     C* Innanzitutto reperisce i dati della prv+loc corrente
089400061017     C                   exsr      rtvloc
089500061017     C*
089600061023     C* 1° obsoletizzo la località BART
089700061017     C                   eval      K_cplVER = loc_ver
089800061017     C                   eval      K_cplNAR = loc_nar
089900061017     C                   eval      K_cplCAP = loc_cap
090000061017     C                   eval      K_cplLOC = loc_loc
090100061017     C     KEYcpl01_C    chain     azcpl01l
090200061017     C                   if        %found(azcpl01l)
090300061023     C                   eval      wCPLLIV = cplLIV
090400061017     C                   movel     'O'           cplFI1
090500061017     C                   movel     *blanks       cplFTR
090600061017     C                   movel     cplESS        wCPLESS
090700061023     C***                movel     *blanks       cplESS
090800080326     C                   eval      cplDTR = *all'8'
090900140612     C                   update(e) azcpl000
091000140611     C*
091100140611     C                   if        not %error
091200140611     C                   eval      wRiga = loc_cap + '  ' + loc_loc +
091300140611     C                             '=> località Obsoletizzata'
091400140611     C                   except    rigaErr
091500140611     C                   endif
091600140611     C*
091700061017     C* Tengo traccia della modifica nel file "diff" - Lato "OLD"
091800061017     C                   eval      wCPCVER = NumPrecVER
091900061017     C                   eval      wCPCCAP = cplCAP
092000061017     C                   exsr      rtvcap
092100061017     C                   eval      diff_cplNAR    = *blanks
092200061017     C                   eval      diff_cplLOC    = cplLOC
092300061017     C                   eval      diff_cpcPRVOLD = cpcPRV
092400061017     C                   eval      diff_cpcPRAOLD = cpcPRA
092500061017     C                   eval      diff_cplCAPOLD = cplCAP
092600061017     C*
092700061017     C                   endif
092800061017     C*
092900061017     C* 2° inserisco nuova loc+cap PT
093000061023     C*
093100061024     C* Se località "oldizzata" era a livello "C" => reperisco i dati d instradamento dal suo cap relativo
093200061024     C                   if        wCPLLIV = 'C'
093300061024     C                   eval      wCPCVER = loc_ver
093400061024     C                   eval      wCPCCAP = loc_cap
093500061024     C                   exsr      cpctocpl
093600061024     C                   endif
093700061023     C*
093800061017     C                   movel     *blanks       cplATB
093900061017     C                   movel     *blanks       cplFI1
094000061017     C                   movel     *blanks       cplFTR
094100061017     C                   movel     wCPLESS       cplESS
094200061023     C* Forzo sempre il nuovo inserimento a livello "L"
094300061023     C                   eval      cplLIV  = 'L'
094400061023     C*
094500061017     C                   eval      cplCAP  = l_cap
094600061017     C                   eval      cplLOC  = l_loc
094700061017     C                   eval      wCPCVER = NumVER
094800061017     C                   eval      wCPCCAP = l_cap
094900061017     C                   exsr      rtvcap
095000061017     C                   if        *in52 = *on
095100080326     C                   eval      cplDTR = *all'9'
095200140611     C                   write(e)  azcpl000
095300140611     C*
095400140611     C                   if        not %error
095500140611     C                   eval      wRiga = l_cap + '  ' + l_loc +
095600140611     C                             '=> località Inserita'
095700140611     C                   except    rigaErr
095800140611     C                   endif
095900140611     C*
096000061024     C* Sempre se località "obsoletizzata" gestita a livello "C"  aggiorno il flag sul cap della località corrente
096100061024     C* che indica la presenza d eccezioni a livello d località
096200061024     C                   if        wCPLLIV = 'C' and cpcESL <> 'S'
096300061024     C                   eval      cpcESL = 'S'
096400080326     C                   eval      cpcDTR = *all'8'
096500140611     C                   update(e) azcpc000
096600061024     C                   endif
096700061017     C* Tengo traccia della modifica nel file "diff" - Lato "NEW"
096800061017     C                   eval      diff_cplNAR    = *blanks
096900061017     C                   eval      diff_cplLOC    = l_loc
097000061017     C                   eval      diff_cpcPRVNEW = cpcPRV
097100061017     C                   eval      diff_cpcPRANEW = cpcPRA
097200061017     C                   eval      diff_cplCAPNEW = cplCAP
097300061017     C                   write(e)  wfdcpl00
097400061017     C                   endif
097500061017     C*
097600140611     C* 3° porto i sinonimi e gli orari della "vecchia" località sulla nuova località
097700061017     C                   eval      K_cpsVER = loc_ver
097800061017     C                   eval      K_cpsNAR = loc_nar
097900061017     C                   eval      K_cpsCAP = loc_cap
098000061017     C                   eval      K_cpsLOC = loc_loc
098100061017     C                   eval      wCPSCAP  = l_cap
098200061017     C                   eval      wCPSLOC  = l_loc
098300061023     C***                exsr      movsin
098400061023     C                   exsr      dupsin
098500140611     C*
098600140611     C                   eval      K_cosVER = K_cpsVER
098700140611     C                   eval      K_cosNAR = K_cpsNAR
098800140611     C                   eval      K_cosCAP = K_cpsCAP
098900140611     C                   eval      K_cosLOC = K_cpsLOC
099000140611     C                   eval      w_cosCAP = wCPSCAP
099100140611     C                   eval      w_cosLOC = wCPSLOC
099200140611     C                   exsr      dupcos
099300061017     C*
099400061017     C* 4° inserisco i sinonimi relativi
099500061017     C                   exsr      exesin
099600061017     C*
099700061017     C                   seton                                        51
099800061017     C*
099900061017     C                   else
100000061017     C*
100100061017     C* Se località nn univoca nella provincia la inserisco a livello d cap
100200061017     C                   if        uniq_cnt > 1
100300061017     C                   exsr      insloc
100400061017     C* ...inserisco anche i sinonimi relativi
100500061017     C                   exsr      exesin
100600061017     C*                  eval      wRiga = l_prv + '  '    +
100700061017     C*                                    l_cap + '  '    +
100800061017     C*                                    l_loc + '     ' +
100900140611     C*                            '=> Località non univoca nella provincia!'
101000061017     C*                  except    rigaErr
101100061017     C                   endif
101200061017     C*
101300061017     C* Se località nn trovata nella provincia la inserisco a livello d cap
101400061017     C                   if        uniq_cnt = 0
101500061017     C                   exsr      insloc
101600061017     C* ...inserisco anche i sinonimi relativi
101700061017     C                   exsr      exesin
101800061017     C*
101900061017     C*                  eval      wRiga = l_prv + '  '    +
102000061017     C*                                    l_cap + '  '    +
102100061017     C*                                    l_loc + '     ' +
102200140611     C*                            '=> Località non trovata nella provincia!'
102300061017     C*                  except    rigaErr
102400061017     C                   endif
102500061017     C                   endif
102600061024     C*
102700061024     C                   else
102800061024     C                   eval      l_FLG2 = 'N'
102900061024     C                   update    loc000
103000061024     C                   eval      wRiga = l_prv + '  '    +
103100061024     C                                     l_cap + '  '    +
103200061024     C                                     l_loc + '     ' +
103300061024     C                             '=> Località su cap inesistente!'
103400061024     C                   except    rigaErr
103500061024     C                   endif
103600061017     C                   endif
103700061017     C*
103800061017     C* Se località PT è stata in qualche modo trattata flaggo record su file PT
103900061017     C                   if        *in51 = *on
104000061017     C                   eval      l_FLG1 = 'S'
104100061017     C                   update(e) loc000
104200061017     C                   endif
104300061017     C*
104400061017     C* Proseguo con la lettura delle località PT
104500061017     C                   read      cap_loc
104600061017     C                   enddo
104700061018     C*
104800061018     C                   EVAL      VID02 = 'Fatto'
104900061018     C                   SETOFF                                       92
105000061018     C                   WRITE     SP20D03
105100061017     C*
105200061017     C                   ENDSR
105300061017     C*--------------------------------------------------------------------------------------------*
105400061017     C* insloc - inserimento nuova loc BART da loc PT
105500061017     C*--------------------------------------------------------------------------------------------*
105600061017     C     insloc        BEGSR
105700061017     C*
105800061017     C                   clear                   azcpl000
105900061017     C                   movel     *blanks       cplATB
106000061017     C                   movel     *blanks       cplFI1
106100061017     C                   movel     *blanks       cplFTR
106200061017     C                   eval      cplVER = NumVER
106300061017     C                   eval      cplNAR = *blanks
106400061017     C                   eval      cplCAP = l_cap
106500061017     C                   eval      cplLOC = l_loc
106600061017     C                   eval      cplLIV = 'C'
106700061017     C                   eval      cplDLV = 'N'
106800061017     C                   eval      cplESS = 'S'
106900061017     C                   eval      wCPCVER = NumVER
107000061017     C                   eval      wCPCCAP = l_cap
107100061017     C                   exsr      rtvcap
107200061017     C                   if        *in52 = *on
107300080326     C                   eval      cplDTR = *all'9'
107400061017     C                   write(e)  azcpl000
107500061017     C                   else
107600061017     C                   eval      l_FLG2 = 'N'
107700061017     C                   update    loc000
107800061017     C                   eval      wRiga = l_prv + '  '    +
107900061017     C                                     l_cap + '  '    +
108000061017     C                                     l_loc + '     ' +
108100061017     C                             '=> Località su cap inesistente!'
108200061017     C                   except    rigaErr
108300061017     C                   endif
108400061017     C*
108500061017     C                   seton                                        51
108600061017     C*
108700061017     C                   ENDSR
108800061017     C*--------------------------------------------------------------------------------------------*
108900061017     C* exesin - gestione sinonimi
109000061017     C*--------------------------------------------------------------------------------------------*
109100061017     C     exesin        BEGSR
109200061017     C*
109300061017     C*
109400061017     C/EXEC SQL
109500061017     C+ declare C1 cursor for
109600061017     C+ select * from cap_sin
109700061017     C+ where PRV = :l_prv and CAP = :l_cap and LOC = :l_loc
109800061017     C+ order by PRV, CAP, LOC, SIN
109900061017     C+ for update of FLG1
110000061017     C/END-EXEC
110100061017     C
110200061017     C/EXEC SQL
110300061017     C+ open C1
110400061017     C/END-EXEC
110500061017     C
110600061017     C/EXEC SQL
110700061017     C+ Fetch next from C1 into :ds_cap_sin
110800061017     C/END-EXEC
110900061017     C*
111000061017     C* Se reperito qualcosa => analizzo
111100061017     C                   dow       sqlcod = *zeros
111200061017     C                   clear                   azcps000
111300061017     C                   eval      cpsVER = NumVER
111400061017     C                   eval      cpsNAR = *blanks
111500061017     C                   eval      cpsCAP = s_cap
111600061017     C                   eval      cpsLOC = s_loc
111700061017     C                   eval      cpsSIN = s_sin
111800061017     C                   eval      cpsFTR = *blanks
111900080326     C                   eval      cpsDTR = *all'9'
112000061017     C                   write(e)  azcps000
112100061017     C
112200061017     C/EXEC SQL
112300061017     C+     update cap_sin
112400061017     C+     set    FLG1 = 'S'
112500061017     C+     where  CURRENT  OF  C1
112600061017     C/END-EXEC
112700061017     C*
112800061017     C*
112900061017     C/EXEC SQL
113000061017     C+ Fetch next from C1 into :ds_cap_sin
113100061017     C/END-EXEC
113200061017     C*
113300061017     C                   enddo
113400061017     C*
113500061017     C/EXEC SQL
113600061017     C+ close C1
113700061017     C/END-EXEC
113800061017     C*
113900061017     C*
114000061017     C                   ENDSR
114100061017     C*--------------------------------------------------------------------------------------------*
114200061017     C* rtvlocuniq - reperimento univocità località x provincia
114300061017     C*--------------------------------------------------------------------------------------------*
114400061017     C     rtvlocuniq    BEGSR
114500061017     C*
114600061017     C/EXEC SQL
114700061017     C+ declare C2 cursor for
114800061024     C+ select cplloc, cpcprv, cpcpra, count(*)
114900061024     C+ from azcpc00f join azcpl00f on
115000061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
115100061024     C+ where cpcver = :NumVER and cpcnar = ' '
115200061024     C+ and (cpcprv = :cpcPRV or
115300061024     C+ (cpcprv = :cpcPRA and cpcprv<>'  ') or
115400061024     C+ cpcpra = :cpcPRV or
115500061024     C+ (cpcpra = :cpcPRA and cpcpra <> '  '))
115600061024     C+ and cplloc = :l_loc
115700061024     C+ group by cplloc, cpcprv, cpcpra
115800061017     C+ for read only
115900061017     C/END-EXEC
116000061017     C
116100061017     C/EXEC SQL
116200061017     C+ open C2
116300061017     C/END-EXEC
116400061017     C
116500061017     C/EXEC SQL
116600061017     C+ Fetch next from C2 into :ds_loc_uniq
116700061017     C/END-EXEC
116800061017     C*
116900061017     C* Se contatore > 1 => località nn univoca nella provincia
117000061017     C                   if        sqlcod = *zeros
117100061017     C                   else
117200061017     C                   eval      uniq_cnt = 0
117300061017     C                   endif
117400061017     C*
117500061017     C/EXEC SQL
117600061017     C+ close C2
117700061017     C/END-EXEC
117800061017     C*
117900061017     C*
118000061017     C                   ENDSR
118100061017     C*--------------------------------------------------------------------------------------------*
118200061017     C* rtvloc - reperimento località
118300061017     C*--------------------------------------------------------------------------------------------*
118400061017     C     rtvloc        BEGSR
118500061017     C*
118600061017     C/EXEC SQL
118700061017     C+ declare C3 cursor for
118800061017     C+ select cplver, cplnar, cplcap, cplloc
118900061017     C+ from azcpc00f join azcpl00f on
119000061017     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
119100061017     C+ where cpcver = :NumVER and cpcnar = ' '
119200061023     C+ and cpcprv = :uniq_prv and cpcpra = :uniq_pra
119300061023     C+ and cplloc = :l_loc
119400061017     C+ for read only
119500061017     C/END-EXEC
119600061017     C
119700061017     C/EXEC SQL
119800061017     C+ open C3
119900061017     C/END-EXEC
120000061017     C
120100061017     C/EXEC SQL
120200061017     C+ Fetch next from C3 into :ds_loc
120300061017     C/END-EXEC
120400061017     C
120500061017     C/EXEC SQL
120600061017     C+ close C3
120700061017     C/END-EXEC
120800061017     C*
120900061017     C                   ENDSR
121000061024     C*--------------------------------------------------------------------------------------------*
121100061024     C* rtvlocnew - reperimento nuova località (appena inserita da PT)
121200061024     C*--------------------------------------------------------------------------------------------*
121300061024     C     rtvlocnew     BEGSR
121400061024     C*
121500061024     C/EXEC SQL
121600061024     C+ declare C10 cursor for
121700061024     C+ select cplver, cplnar, cplcap, cplloc
121800061024     C+ from azcpc00f join azcpl00f on
121900061024     C+ cpcver = cplver and cpcnar = cplnar and cpccap = cplcap
122000061024     C+ where cpcver = :NumVER and cpcnar = ' '
122100061024     C+ and cpcprv = :prv_prv and cpcpra = :prv_pra
122200061024     C+ and cplloc = :l2_cplLOC and cplfi1 = '  ' and cplftr = ' '
122300061024     C+ for read only
122400061024     C/END-EXEC
122500061024     C
122600061024     C/EXEC SQL
122700061024     C+ open C10
122800061024     C/END-EXEC
122900061024     C
123000061024     C/EXEC SQL
123100061024     C+ Fetch next from C10 into :ds_loc
123200061024     C/END-EXEC
123300061024     C
123400061024     C/EXEC SQL
123500061024     C+ close C10
123600061024     C/END-EXEC
123700061024     C*
123800061024     C                   ENDSR
123900061024     C*--------------------------------------------------------------------------------------------*
124000061024     C* rtvprv - reperimento provincia da cap + loc
124100061024     C*--------------------------------------------------------------------------------------------*
124200061024     C     rtvprv        BEGSR
124300061024     C*
124400061024     C/EXEC SQL
124500061024     C+ declare C9 cursor for
124600061024     C+ select cpcprv, cpcpra
124700061024     C+ from azcpc00f
124800061024     C+ where cpcver = :NumVER and cpcnar = ' '
124900061024     C+ and cpccap = :l2_cplCAP
125000061024     C+ for read only
125100061024     C/END-EXEC
125200061024     C
125300061024     C/EXEC SQL
125400061024     C+ open C9
125500061024     C/END-EXEC
125600061024     C
125700061024     C/EXEC SQL
125800061024     C+ Fetch next from C9 into :ds_prv
125900061024     C/END-EXEC
126000061024     C
126100061024     C/EXEC SQL
126200061024     C+ close C9
126300061024     C/END-EXEC
126400061024     C*
126500061024     C                   ENDSR
126600061017     C*--------------------------------------------------------------------------------------------*
126700061017     C* rtvcap - reperimento cap
126800061017     C*--------------------------------------------------------------------------------------------*
126900061017     C     rtvcap        BEGSR
127000061017     C*
127100061017     C                   setoff                                       52
127200061017     C*
127300061017     C* Verifico se trattasi d cap già esistente
127400061017     C                   eval      K_cpcVER = wCPCVER
127500061017     C                   eval      K_cpcNAR = *blanks
127600061017     C                   eval      K_cpcCAP = wCPCCAP
127700061017     C     KEYcpc01_C    chain     azcpc01l
127800061017     C                   if        %found(azcpc01l)
127900061017     C                   seton                                        52
128000061017     C                   endif
128100061017     C*
128200061017     C                   ENDSR
128300061017     C*--------------------------------------------------------------------------------------------*
128400061017     C* rtvcappt - reperimento cap PT
128500061017     C*--------------------------------------------------------------------------------------------*
128600061017     C     rtvcappt      BEGSR
128700061017     C*
128800061018     C                   setoff                                       54
128900061017     C*
129000061017     C/EXEC SQL
129100061017     C+ declare C4 cursor for
129200061017     C+ select * from cap_cap
129300061018     C+ where cap = :cpcCAP
129400061017     C+ for read only
129500061017     C/END-EXEC
129600061017     C
129700061017     C/EXEC SQL
129800061017     C+ open C4
129900061017     C/END-EXEC
130000061017     C
130100061017     C/EXEC SQL
130200061017     C+ Fetch next from C4 into :ds_cap_pt
130300061017     C/END-EXEC
130400061017     C*
130500061018     C* Se nn trovato => segnalo con indicatore 54
130600061017     C                   if        sqlcod <> *zeros
130700061018     C                   seton                                        54
130800061017     C                   endif
130900061017     C
131000061017     C/EXEC SQL
131100061017     C+ close C4
131200061017     C/END-EXEC
131300061017     C*
131400061017     C                   ENDSR
131500061017     C*--------------------------------------------------------------------------------------------*
131600061017     C* rtvlocpt - reperimento località PT
131700061017     C*--------------------------------------------------------------------------------------------*
131800061017     C     rtvlocpt      BEGSR
131900061017     C*
132000061017     C                   setoff                                       53
132100061017     C*
132200061017     C/EXEC SQL
132300061017     C+ declare C5 cursor for
132400061017     C+ select * from cap_loc
132500061024     C+ where cap = :cplCAP and substr(loc, 1, 35) = :cplLOC
132600061017     C+ for read only
132700061017     C/END-EXEC
132800061017     C
132900061017     C/EXEC SQL
133000061017     C+ open C5
133100061017     C/END-EXEC
133200061017     C
133300061017     C/EXEC SQL
133400061017     C+ Fetch next from C5 into :ds_loc_pt
133500061017     C/END-EXEC
133600061017     C*
133700061017     C* Se nn trovato => segnalo con indicatore 53
133800061017     C                   if        sqlcod <> *zeros
133900061017     C                   seton                                        53
134000061017     C                   endif
134100061017     C
134200061017     C/EXEC SQL
134300061017     C+ close C5
134400061017     C/END-EXEC
134500061017     C*
134600061017     C                   ENDSR
134700061017     C*--------------------------------------------------------------------------------------------*
134800061017     C* rtvcompt - reperimento comune PT
134900061017     C*--------------------------------------------------------------------------------------------*
135000061017     C     rtvcompt      BEGSR
135100061017     C*
135200061017     C                   Z-ADD     *zeros        com_cnt           9 0
135300061017     C*
135400061017     C/EXEC SQL
135500061017     C+ declare C6 cursor for
135600061017     C+ select com from cap_loc
135700061017     C+ where cap = :rainer_cap and loc = com
135800061017     C+ group by com
135900061017     C+ for read only
136000061017     C/END-EXEC
136100061017     C
136200061017     C/EXEC SQL
136300061017     C+ open C6
136400061017     C/END-EXEC
136500061017     C
136600061017     C/EXEC SQL
136700061017     C+ Fetch next from C6 into :ds_com_pt
136800061017     C/END-EXEC
136900061017     C*
137000061017     C* Se reperito qualcosa => analizzo e conto
137100061017     C                   dow       sqlcod = *zeros
137200061017     C                   add       1             com_cnt
137300061017     C*
137400061017     C/EXEC SQL
137500061017     C+ Fetch next from C6 into :ds_com_pt
137600061017     C/END-EXEC
137700061017     C*
137800061017     C                   enddo
137900061017     C*
138000061017     C/EXEC SQL
138100061017     C+ close C6
138200061017     C/END-EXEC
138300061017     C*
138400061017     C                   ENDSR
138500061017     C*--------------------------------------------------------------------------------------------*
138600061017     C* rtvlocrainer - reperimento località x provincia su file "Raineri"
138700061017     C*--------------------------------------------------------------------------------------------*
138800061017     C     rtvlocrainer  BEGSR
138900061017     C*
139000061017     C/EXEC SQL
139100061017     C+ declare C7 cursor for
139200061017     C+ select loc, min(cap), count(*)
139300061017     C+ from cap_rainer
139400061017     C+ where (prv = :cpcPRV or prv = :cpcPRA)
139500061024     C+ and substr(loc, 1, 35) = :cplLOC
139600061017     C+ group by loc
139700061017     C+ for read only
139800061017     C/END-EXEC
139900061017     C
140000061017     C/EXEC SQL
140100061017     C+ open C7
140200061017     C/END-EXEC
140300061017     C
140400061017     C/EXEC SQL
140500061017     C+ Fetch next from C7 into :ds_loc_rainer
140600061017     C/END-EXEC
140700061017     C*
140800061017     C* Se contatore > 1 => località nn univoca nella provincia
140900061017     C                   if        sqlcod = *zeros
141000061017     C                   else
141100061017     C                   eval      rainer_cnt = 0
141200061017     C                   endif
141300061017     C*
141400061017     C/EXEC SQL
141500061017     C+ close C7
141600061017     C/END-EXEC
141700061017     C*
141800061017     C*
141900061017     C                   ENDSR
142000061017     C*--------------------------------------------------------------------------------------------*
142100061017     C* movsin - spostamento sinonimi da "vecchia" località a "nuova" località
142200061017     C*--------------------------------------------------------------------------------------------*
142300061017     C     movsin        BEGSR
142400061017     C*
142500061017     C     KEYcps01_P    setll     azcps01l
142600061017     C                   if        %equal(azcps01l)
142700061017     C     KEYcps01_P    reade     azcps01l
142800061017     C                   dow       not %eof(azcps01l)
142900061017     C                   eval      cpsCAP = wCPSCAP
143000061017     C                   eval      cpsLOC = wCPSLOC
143100061017     C                   eval      cpsFTR = *blanks
143200061023     C* Solo se sinonimo corrente diverso da località sinonimata
143300061023     C                   if        cpsSIN <> cpsLOC
143400080326     C                   eval      cpsDTR = *all'8'
143500061017     C                   update(e) azcps000
143600061023     C                   endif
143700061017     C     KEYcps01_P    reade     azcps01l
143800061017     C                   enddo
143900061017     C                   endif
144000061017     C*
144100061017     C                   ENDSR
144200061017     C*--------------------------------------------------------------------------------------------*
144300061017     C* dupsin - copia sinonimi da "vecchia" località a "nuova" località
144400061017     C*--------------------------------------------------------------------------------------------*
144500061017     C     dupsin        BEGSR
144600061017     C*
144700061017     C     KEYcps01_P    setll     azcps01l
144800061017     C                   if        %equal(azcps01l)
144900061017     C     KEYcps01_P    reade     azcps01l
145000061017     C                   dow       not %eof(azcps01l)
145100061017     C                   eval      cpsCAP = wCPSCAP
145200061017     C                   eval      cpsLOC = wCPSLOC
145300061017     C                   eval      cpsFTR = *blanks
145400061023     C* Solo se sinonimo corrente diverso da località sinonimata
145500061023     C                   if        cpsSIN <> cpsLOC
145600080326     C                   eval      cpsDTR = *all'9'
145700061017     C                   write(e)  azcps000
145800061023     C                   endif
145900061017     C     KEYcps01_P    reade     azcps01l
146000061017     C                   enddo
146100061017     C                   endif
146200061017     C*
146300061017     C                   ENDSR
146400140611     C*--------------------------------------------------------------------------------------------*
146500140611     C* dupcos - copia orari da "vecchia" località a "nuova" località
146600140611     C*--------------------------------------------------------------------------------------------*
146700140611     C     dupcos        BEGSR
146800140611     C*
146900140611     C     KEYcos01_P    setll     azcos01l
147000140611     C                   if        %equal(azcos01l)
147100140611     C     KEYcos01_P    reade     azcos01l
147200140611     C                   dow       not %eof(azcos01l)
147300140611     C                   eval      cosCAP = w_cosCAP
147400140611     C                   eval      cosLOC = w_cosLOC
147500140611     C                   eval      cosDUV = %dec(%date() :*ISO)
147600140611     C                   eval      cosUTE = 'EDP'
147700140611     C                   write(e)  azcos000
147800140611     C     KEYcos01_P    reade     azcos01l
147900140611     C                   enddo
148000140611     C                   endif
148100140611     C*
148200140611     C                   ENDSR
148300061017     C*--------------------------------------------------------------------------------------------*
148400061017     C* normloc - "normalizzazione" sintassi località BART
148500061017     C*--------------------------------------------------------------------------------------------*
148600061017     C     normloc       BEGSR
148700061017     C*
148800061017     C     NumVER        setll     azcpl01l
148900061017     C                   if        %equal(azcpl01l)
149000061017     C     NumVER        reade     azcpl01l
149100061017     C                   dow       not %eof(azcpl01l)
149200061017     C*
149300061017     C* Verifico presenza caratteri da eliminare
149400061017     C                   setoff                                       45
149500061017     C                   eval      wCPLLOC = cplLOC
149600061017     C                   if        %scan('''':cplLOC) > *zeros
149700061017     C     '''':' '      xlate     cplLOC        cplLOC
149800061017     C                   seton                                        45
149900061017     C                   endif
150000061017     C*
150100061017     C                   if        %scan('"':cplLOC)  > *zeros
150200061017     C     '"':' '       xlate     cplLOC        cplLOC
150300061017     C                   seton                                        45
150400061017     C                   endif
150500061017     C*
150600061017     C                   if        %scan('.':cplLOC)  > *zeros
150700061017     C     '.':' '       xlate     cplLOC        cplLOC
150800061017     C                   seton                                        45
150900061017     C                   endif
151000061017     C*
151100061017     C                   if        %scan('-':cplLOC)  > *zeros
151200061017     C     '-':' '       xlate     cplLOC        cplLOC
151300061017     C                   seton                                        45
151400061017     C                   endif
151500061017     C*
151600061017     C* Se sono state effettuate sostituzioni => gestisco sia località che sinonimi relativi
151700061017     C                   if        *in45 = *on
151800061017     C*
151900061017     C* Compatto gli eventuali spazi ripetuti consecutivamente
152000061017     C                   eval      cplLOC = %trim(cplLOC)
152100061017     C                   z-add     1             wPos              2 0
152200061017     C                   eval      wPos = %scan('  ':cplLOC:wPos)
152300061017     C                   dow       wPos > *zeros AND wPos < %len(%trim(cplLOC))
152400061017     C                   eval      cplLOC = %subst(cplLOC:1:wPos)+
152500061017     C                                      %subst(cplLOC:wPos+2)
152600061017     C                   eval      wPos = %scan('  ':cplLOC:wPos+1)
152700061017     C                   enddo
152800061017     C*
152900061017     C* 1° Sostituisco la località "normalizzata" a quella originale
153000061017     C                   eval      cplFTR = *blanks
153100080326     C                   eval      cplDTR = *all'8'
153200061017     C                   update(e) azcpl000
153300061017     C*
153400061017     C* 2° Inserisco come sinonimo della località "normalizzata" la località originale
153500061017     C                   clear                   azcps000
153600061017     C                   eval      cpsVER = cplVER
153700061017     C                   eval      cpsNAR = cplNAR
153800061017     C                   eval      cpsCAP = cplCAP
153900061017     C                   eval      cpsLOC = cplLOC
154000061017     C                   eval      cpsSIN = wCPLLOC
154100061017     C                   eval      cpsFTR = *blanks
154200080326     C                   eval      cpsDTR = *all'9'
154300061017     C                   write(e)  azcps000
154400061017     C*
154500061017     C* 3° Sposto i sinonimi della località originale su quella "normalizzata"
154600061017     C                   eval      K_cpsVER = cplVER
154700061017     C                   eval      K_cpsNAR = cplNAR
154800061017     C                   eval      K_cpsCAP = cplCAP
154900061017     C                   eval      K_cpsLOC = wCPLLOC
155000061017     C                   eval      wCPSCAP  = cplCAP
155100061017     C                   eval      wCPSLOC  = cplLOC
155200061018     C                   exsr      movsin
155300061017     C*
155400061017     C                   endif
155500061017     C*
155600061017     C* Proseguo con la lettura delle località BART
155700061017     C     NumVER        reade     azcpl01l
155800061017     C                   enddo
155900061017     C                   endif
156000061017     C*
156100061017     C                   ENDSR
156200061017     C*--------------------------------------------------------------------------------------------*
156300061017     C* exever1 - Esecuzione 1° verifica: cap BART no in cap PT => obsoletare cap BART
156400061017     C*--------------------------------------------------------------------------------------------*
156500061017     C     exever1       BEGSR
156600061018     C*
156700061018     C                   EVAL      VID03 = 'In elaborazione'
156800061018     C                   SETON                                        93
156900061018     C                   WRITE     SP20D03
157000061017     C*
157100061017     C* Scorro tutto il file dei cap BART
157200061018     C                   eval      K_cpcVER = NumVER
157300061018     C                   eval      K_cpcNAR = *blanks
157400061017     C     *start        setll     azcpc01l
157500061018     C     KEYcpc01_P    setll     azcpc01l
157600061018     C     KEYcpc01_P    reade     azcpc01l
157700061017     C                   dow       not %eof(azcpc01l)
157800061017     C*
157900061017     C* Se cap nn già obsoleto effettuo le verifiche
158000061018     C                   if        cpcFI1 = *blanks           AND
158100061018     C                             %subst(cpcCAP:4:2) <> '00' AND
158200061018     C                             %subst(cpcCAP:4:2) <> '  '
158300061017     C*
158400061017     C* Verifico se cap BART è in file cap PT
158500061018     C                   exsr      rtvcappt
158600061017     C*
158700061017     C* Se nn trovata flaggo in maniera "evidente" cap corrente BART
158800061018     C                   if        *in54 = *on
158900061018     C                   eval      cpcFI1 = '?'
159000080326     C                   eval      cpcDTR = *all'8'
159100061017     C                   update(e) azcpc000
159200061017     C                   endif
159300061017     C*
159400061017     C                   endif
159500061017     C*
159600061017     C* Proseguo con la lettura dei cap BART
159700061018     C     KEYcpc01_P    reade     azcpc01l
159800061017     C                   enddo
159900061018     C*
160000061018     C                   EVAL      VID03 = 'Fatto'
160100061018     C                   SETOFF                                       93
160200061018     C                   WRITE     SP20D03
160300061017     C*
160400061017     C                   ENDSR
160500061017     C*--------------------------------------------------------------------------------------------*
160600061017     C* exever2 - Esecuzione 2° verifica: loc BART no in loc PT => obsoletare loc BART
160700061017     C*--------------------------------------------------------------------------------------------*
160800061017     C     exever2       BEGSR
160900061018     C*
161000061018     C                   EVAL      VID04 = 'In elaborazione'
161100061018     C                   SETON                                        94
161200061018     C                   WRITE     SP20D03
161300061017     C*
161400061017     C* Scorro tutto il file delle località BART
161500061017     C     *start        setll     azcpl01l
161600061018     C     NumVER        setll     azcpl01l
161700061017     C     NumVER        reade     azcpl01l
161800061017     C                   dow       not %eof(azcpl01l)
161900061017     C*
162000061017     C                   setoff                                       60
162100061017     C                   setoff                                       61
162200061017     C*
162300061017     C* Se impostato flag "esistenza sinonimi" => verifica che sia vero
162400061017     C                   if        cplESS = 'S'
162500061017     C                   eval      cplESS = *blanks
162600061017     C                   seton                                        70
162700061017     C                   eval      K_cpsVER = NumVER
162800061017     C                   eval      K_cpsNAR = *blanks
162900061017     C                   eval      K_cpsCAP = cplCAP
163000061017     C                   eval      K_cpsLOC = cplLOC
163100061017     C     KEYcps01_P    setll     azcps01l
163200061017     C                   if        %equal(azcps01l)
163300061017     C     KEYcps01_P    reade     azcps01l
163400061017     C                   dow       not %eof(azcps01l)
163500061017     C                   if        cpsATB <> 'A'
163600061017     C                   eval      cplESS = 'S'
163700061017     C                   setoff                                       70
163800061017     C                   leave
163900061017     C                   endif
164000061017     C     KEYcps01_P    reade     azcps01l
164100061017     C                   enddo
164200061017     C                   endif
164300061017     C                   endif
164400061017     C*
164500061017     C* Se località nn già obsoleta effettuo le verifiche
164600061018     C                   if        cplFI1 = *blanks           AND
164700061018     C                             %subst(cplCAP:4:2) <> '00' AND
164800061018     C                             %subst(cplCAP:4:2) <> '  '
164900061017     C*
165000061017     C* Verifico se località BART presente in località PT
165100061017     C                   exsr      rtvlocpt
165200061017     C*
165300061017     C* Se nn trovata
165400061017     C                   if        *in53 = *on
165500061017     C                   seton                                        61
165600061017     C*
165700061017     C* Reperisco la provincia dal cap BART
165800061017     C                   eval      K_cpcVER = NumVER
165900061017     C                   eval      K_cpcNAR = *blanks
166000061017     C                   eval      K_cpcCAP = cplCAP
166100061017     C     KEYcpc01_C    chain     azcpc01l
166200061017     C                   if        %found(azcpc01l)
166300061017     C*
166400061017     C* Reperisco il cap PT dal file "Raineri" associato alla loc BART corrente
166500061017     C                   exsr      rtvlocrainer
166600061017     C*
166700061017     C* Se trovata e univoca procedo
166800061017     C                   if        rainer_cnt = 1
166900061017     C*
167000061017     C* Reperisco il comune PT associato al cap della loc BART corrente
167100061017     C                   exsr      rtvcompt
167200061017     C*
167300061017     C* Se trovata e univoco procedo
167400061017     C                   if        com_cnt = 1
167500061017     C*
167600061017     C                   seton                                        60
167700061017     C*
167800061017     C* Gestisco inserendo la località coerrente come sinonimo del comune cui nel file cap PT
167900061017     C* il cap della località BART corrente è associato, x cui =>
168000061017     C*
168100061017     C* 1° Annullo la località corrente
168200061017     C                   eval      cplFI1 = '!'
168300080326     C                   eval      cplDTR = *all'8'
168400061017     C                   update(e) azcpl000
168500061017     C*
168600061017     C* Inserisco come sinonimo del comune la località BART corrente (appena annullata)
168700061017     C                   clear                   azcps000
168800061017     C                   eval      cpsVER = cplVER
168900061017     C                   eval      cpsNAR = cplNAR
169000061017     C                   eval      cpsCAP = rainer_cap
169100061017     C                   eval      cpsLOC = com_com
169200061017     C                   eval      cpsSIN = cplLOC
169300061017     C                   eval      cpsFTR = *blanks
169400080326     C                   eval      cpsDTR = *all'9'
169500061017     C                   write(e)  azcps000
169600061017     C*
169700061017     C* Quindi sposto i sinonimi della località originale sul comune appena reperito
169800061017     C                   eval      K_cpsVER = cplVER
169900061017     C                   eval      K_cpsNAR = cplNAR
170000061017     C                   eval      K_cpsCAP = cplCAP
170100061017     C                   eval      K_cpsLOC = cplLOC
170200061017     C                   eval      wCPSCAP  = rainer_cap
170300061017     C                   eval      wCPSLOC  = com_com
170400061018     C***                exsr      movsin
170500061018     C                   exsr      dupsin
170600140611     C*
170700140611     C                   eval      K_cosVER = K_cpsVER
170800140611     C                   eval      K_cosNAR = K_cpsNAR
170900140611     C                   eval      K_cosCAP = K_cpsCAP
171000140611     C                   eval      K_cosLOC = K_cpsLOC
171100140611     C                   eval      w_cosCAP = wCPSCAP
171200140611     C                   eval      w_cosLOC = wCPSLOC
171300140611     C                   exsr      dupcos
171400061017     C*
171500061017     C                   endif
171600061017     C                   endif
171700061017     C                   endif
171800061017     C*
171900061017     C* Se nn trovata flaggo in maniera "evidente" cap+loc corrente BART
172000061017     C                   if        *in60 = *off
172100061017     C                   eval      cplFI1 = '?'
172200061017     C                   endif
172300061017     C*
172400061017     C                   endif
172500061017     C*
172600061017     C                   endif
172700061017     C*
172800061017     C* Se occorre updatare => eseguo
172900061017     C                   if        (*in60 = *off AND *in61 = *on) OR
173000061017     C                             *in70
173100061017     C                   eval      cplFTR = *blanks
173200080326     C                   eval      cplDTR = *all'8'
173300061017     C                   update(e) azcpl000
173400061017     C                   endif
173500061017     C*
173600061017     C* Proseguo con la lettura delle località BART
173700061017     C     NumVER        reade     azcpl01l
173800061017     C                   enddo
173900061018     C*
174000061018     C                   EVAL      VID04 = 'Fatto'
174100061018     C                   SETOFF                                       94
174200061018     C                   WRITE     SP20D03
174300061017     C*
174400061017     C                   ENDSR
174500061017     C*--------------------------------------------------------------------------------------------*
174600061123     C* exever3 - Esecuzione 3° verifica: eliminare sin BART che sono già loc BART (nn obs) x lo stesso cap
174700061017     C*--------------------------------------------------------------------------------------------*
174800061017     C     exever3       BEGSR
174900061018     C*
175000061023     C                   EVAL      VID07 = 'In elaborazione'
175100061023     C                   SETON                                        97
175200080326     C                   WRITE     SP20D03
175300061017     C*
175400061017     C                   eval      wSQL =
175500061017     C                             'update azcps00f set cpsatb = ''A'', '    +
175600080326     C                             'cpsftr='' '', cpsdtr=88888888 '          +
175700080326     C                             ' where cpsver=' + VIDVER                 +
175800061017     C                             ' and cpscap!!cpssin in (select '         +
175900061017     C                             'cplcap!!cplloc from azcpl00f '           +
176000061017     C                             'where cplver=' + VIDVER + ' and cplFI1 ' +
176100061123     C                             '= '' '')'
176200061017     C*
176300061017     C/EXEC SQL
176400061017     C+ execute immediate: wSQL
176500061017     C/END-EXEC
176600061017     C*
176700061023     C                   EVAL      VID07 = 'Fatto'
176800061023     C                   SETOFF                                       97
176900061018     C                   WRITE     SP20D03
177000061018     C*
177100061017     C                   ENDSR
177200061017     C*------------------------------------------------------------------------*
177300061017     C* exever4 - Esecuzione 4° verifica: obsoleta cap BART aventi tutte relative loc BART obsolete
177400061017     C*------------------------------------------------------------------------*
177500061017     C     exever4       BEGSR
177600061018     C*
177700061018     C                   EVAL      VID06 = 'In elaborazione'
177800061018     C                   SETON                                        96
177900061018     C                   WRITE     SP20D03
178000061017     C*
178100061017     C/EXEC SQL
178200061017     C+ declare C8 cursor for select * from azcpl00f
178300061017     C+ where cplver = :NumVER and cplnar = ' '
178400061017     C+ order by cplcap, cplloc
178500061017     C+ for fetch only
178600061017     C/END-EXEC
178700061017     C
178800061017     C/EXEC SQL
178900061017     C+ open C8
179000061017     C/END-EXEC
179100061017     C
179200061017     C/EXEC SQL
179300061017     C+ Fetch C8 into :ds_cpl_obs
179400061017     C/END-EXEC
179500061017     C
179600061017     C                   dow       sqlcod=*zeros
179700061017     C*
179800061017     C* ESEGUO CONSIDERAZIONI ED OPERAZIONI DI SCRITTURA ENTRO CICLO
179900061017     C                   if        O_cplcap = savcap
180000061017     C                   if        O_cplfi1 = *blanks
180100061017     C                   movel     '0'           chkobs
180200061017     C                   endif
180300061017     C                   else
180400061017     C                   if        chkobs = '1' and savcap <> *blanks
180500061017     C                   eval      K_cpcVER = NumVER
180600061017     C                   eval      K_cpcNAR = *blanks
180700061017     C                   eval      K_cpcCAP = savcap
180800061017     C     KEYcpc01_C    chain     azcpc01l
180900061017     C                   if        %found(azcpc01l) AND cpcFI1 <> 'O'
181000061017     C                   movel     'O '          cpcFI1
181100061017     C                   movel     *blanks       cpcFTR
181200080326     C                   eval      cpcDTR = *all'8'
181300061017     C                   update(e) azcpc000
181400061017     C                   endif
181500061017     C                   endif
181600061017     C                   movel     O_cplcap      savcap
181700061017     C                   movel     '1'           chkobs
181800061017     C                   if        O_cplfi1 = *blanks
181900061017     C                   movel     '0'           chkobs
182000061017     C                   endif
182100061017     C                   endif
182200061017     C
182300061017     C/EXEC SQL
182400061017     C+ Fetch C8 into :ds_cpl_obs
182500061017     C/END-EXEC
182600061017     C
182700061017     C                   enddo
182800061017     C
182900061017     C/EXEC SQL
183000061017     C+ close C8
183100061017     C/END-EXEC
183200061018     C*
183300061018     C                   EVAL      VID06 = 'Fatto'
183400061018     C                   SETOFF                                       96
183500061018     C                   WRITE     SP20D03
183600061017     C*
183700061017     C                   ENDSR
183800061023     C*--------------------------------------------------------------------------------------------*
183900061023     C* exever5 - Esecuzione 5° verifica: vaglia loc BART "?" tramite TISI95R
184000061023     C*--------------------------------------------------------------------------------------------*
184100061023     C     exever5       BEGSR
184200061023     C*
184300061023     C                   EVAL      VID05 = 'In elaborazione'
184400061023     C                   SETON                                        95
184500061023     C                   WRITE     SP20D03
184600061023     C*
184700061023     C* Scorro tutto il file delle località BART
184800061023     C     *start        setll     azcpl01l
184900061023     C     NumVER        setll     azcpl01l
185000061023     C     NumVER        reade     azcpl01l
185100061023     C                   dow       not %eof(azcpl01l)
185200061024     C*
185300061024     C                   setoff                                       58
185400061023     C*
185500061023     C* Se località già battezzata "?" elaboro
185600061023     C                   if        %trim(cplFI1) = '?'
185700061023     C*
185800061023     C* Reperisco la provincia dal cap BART
185900061023     C                   eval      K_cpcVER = NumVER
186000061023     C                   eval      K_cpcNAR = *blanks
186100061023     C                   eval      K_cpcCAP = cplCAP
186200061023     C     KEYcpc01_C    chain     azcpc01l
186300061023     C                   if        %found(azcpc01l)
186400061023     C*
186500061023     C* Passo al TISI95R loc + prv e se livello affidabilità >= 2 listo e tengo traccia aggiornando la località
186600061023     C                   clear                   TISI95DS
186700061023     C                   eval      I95TCN = '7'
186800061023     C                   z-add     datcor        I95DAT
186900061023     C                   eval      I95LOC = cplLOC
187000061023     C                   eval      I95PRV = cpcPRV
187100061023     C                   call      'TISI95R'
187200061023     C                   parm                    TISI95DS
187300061023     C*
187400061023     C                   if        O95LIA >= '2'
187500061024     C*
187600061024     C                   seton                                        58
187700061023     C*
187800061023     C* 1° Listo in stampa x preventiva verifica manuale
187900061023     C                   eval      wSI95 = 'DA: ' +
188000061023     C                                     cplCAP + ' ' +
188100061023     C                                     cplLOC + ' ' +
188200061023     C                                     cpcPRV + ' ' +
188300061023     C                                     ' => A: ' +
188400061023     C                                     O95CAP + ' ' +
188500061023     C                                     O95LOC + ' ' +
188600061023     C                                     O95PRV + ' ' +
188700061023     C                                     O95SIN + ' ' +
188800061023     C                                     O95LID + ' ' +
188900061023     C                                     O95LIA
189000061023     C                   except    rigaSI95
189100061023     C*
189200061023     C* 2° Marco la località corrente (solo se lancio no in simulazione)
189300061023     C                   if        VIDSI95 = 'X'
189400061023     C                   eval      cplFI1  = 'T'
189500080326     C                   eval      cplDTR = *all'8'
189600061023     C                   update(e) azcpl000
189700061023     C*
189800061023     C* 3° Sposto i sinonimi della località originale sul comune appena reperito
189900061023     C                   eval      K_cpsVER = cplVER
190000061023     C                   eval      K_cpsNAR = cplNAR
190100061023     C                   eval      K_cpsCAP = cplCAP
190200061023     C                   eval      K_cpsLOC = cplLOC
190300061204     C                   eval      wCPSCAP  = O95CAP
190400061204     C                   eval      wCPSLOC  = O95LOC
190500061023     C***                exsr      movsin
190600061024     C                   exsr      dupsin
190700140611     C*
190800140611     C                   eval      K_cosVER = K_cpsVER
190900140611     C                   eval      K_cosNAR = K_cpsNAR
191000140611     C                   eval      K_cosCAP = K_cpsCAP
191100140611     C                   eval      K_cosLOC = K_cpsLOC
191200140611     C                   eval      w_cosCAP = wCPSCAP
191300140611     C                   eval      w_cosLOC = wCPSLOC
191400140611     C                   exsr      dupcos
191500140611     C*
191600061023     C                   endif
191700061023     C*
191800061023     C                   endif
191900061023     C                   endif
192000061023     C                   endif
192100061024     C*
192200061024     C* Se x qualunque ragione nn ho elaborato => listo
192300061024     C***                if        *in58 = *off
192400061024     C***                eval      wSI95 = 'DA: ' +
192500061024     C***                                  cplCAP + ' ' +
192600061024     C***                                  cplLOC + ' ' +
192700061024     C***                                  cpcPRV + ' ' +
192800061024     C***                                  ' => N.P. '
192900061024     C***                except    rigaSI95
193000061024     C***                endif
193100061023     C*
193200061023     C* Proseguo con la lettura delle località BART
193300061023     C     NumVER        reade     azcpl01l
193400061023     C                   enddo
193500061023     C*
193600061023     C                   EVAL      VID05 = 'Fatto'
193700061023     C                   SETOFF                                       95
193800061023     C                   WRITE     SP20D03
193900061023     C*
194000061023     C                   ENDSR
194100061024     C*--------------------------------------------------------------------------------------------*
194200061024     C* exever6 - Esecuzione 6° verifica: sistema le LOC BART obsolete e a livello "C" x
194300061024     C*                                   ricongiunzione a nuova località su diverso cap  ma in
194400061024     C*                                   stessa provincia / provincia alternativa
194500061024     C*--------------------------------------------------------------------------------------------*
194600061024     C     exever6       BEGSR
194700061024     C*
194800061024     C                   EVAL      VID07 = 'In elaborazione'
194900061024     C                   SETON                                        97
195000061024     C                   WRITE     SP20D03
195100061024     C*
195200061024     C* Scorro tutto il file delle località BART
195300061024     C     *start        setll     azcpl02l
195400061024     C     NumVER        setll     azcpl02l
195500061024     C     NumVER        reade     azcpl02l
195600061024     C                   dow       not %eof(azcpl02l)
195700061024     C*
195800061024     C* Elaboro località obsolete e a livello "C"
195900061024     C                   if        l2_cplFI1 = 'O ' AND
196000061024     C                             l2_cplLIV = 'C'
196100061024     C*
196200061024     C* Reperisco innanzitutto la provincia / prov alternativa della località corrente
196300061024     C                   clear                   ds_prv
196400061024     C                   exsr      rtvprv
196500061024     C*
196600061024     C* Se reperito qualcosa proseguo con elaborazione
196700061024     C                   if        ds_prv <> *blanks
196800061024     C*
196900061024     C* Quindi reperisco la località precisa
197000061024     C                   clear                   ds_loc
197100061024     C                   exsr      rtvlocnew
197200061024     C*
197300061024     C* Se reperito qualcosa proseguo con elaborazione
197400061024     C                   if        ds_loc <> *blanks
197500061024     C*
197600061024     C* Quindi aggancio la nuova località inserita da PT
197700061024     C                   eval      K_cplVER = loc_ver
197800061024     C                   eval      K_cplNAR = loc_nar
197900061024     C                   eval      K_cplCAP = loc_cap
198000061024     C                   eval      K_cplLOC = loc_loc
198100061024     C     KEYcpl01_C    chain     azcpl01l
198200061024     C                   if        %found(azcpl01l)
198300061024     C*
198400061024     C* Reperisco i dati d instradamento dal cap della relativa località "obsoletizzata"
198500061024     C                   eval      wCPCVER = l2_cplVER
198600061024     C                   eval      wCPCCAP = l2_cplCAP
198700061024     C                   exsr      cpctocpl
198800061024     C*
198900061024     C* Imposto quindi a "L" il livello d gestioine della nuova località inserita da PT
199000061024     C                   eval      cplLIV  = 'L'
199100061024     C*
199200061024     C* Superfluo ma cmq sfleggo il record
199300061024     C                   movel     *blanks       cplFTR
199400061024     C*
199500061024     C* Quindi aggiorno il record della nuova località inserita da PT
199600080326     C                   eval      cplDTR = *all'8'
199700061024     C                   update(e) azcpl000
199800061024     C*
199900061024     C* A questo punto occorre indicare sul cap della nuova localita inserita da PT la presenza d eccezioni x localita
200000061024     C                   eval      wCPCVER = loc_ver
200100061024     C                   eval      wCPCCAP = loc_cap
200200061024     C                   exsr      rtvcap
200300061024     C                   if        *in52 = *on
200400061024     C                   eval      cpcESL = 'S'
200500080326     C                   eval      cpcDTR = *all'8'
200600061024     C                   update(e) azcpc000
200700061024     C                   endif
200800061024     C                   endif
200900061024     C                   endif
201000061024     C                   endif
201100061024     C                   endif
201200061024     C*
201300061024     C* Proseguo con la lettura delle località BART
201400061024     C     NumVER        reade     azcpl02l
201500061024     C                   enddo
201600061024     C*
201700061024     C                   EVAL      VID07 = 'Fatto'
201800061024     C                   SETOFF                                       97
201900061024     C                   WRITE     SP20D03
202000061024     C*
202100061122     C                   ENDSR
202200061122     C*--------------------------------------------------------------------------------------------*
202300061122     C* exever8 - Esecuzione 8° verifica: annullaamento sinonimi doppi su stesso cap
202400061122     C*--------------------------------------------------------------------------------------------*
202500061122     C     exever8       BEGSR
202600061122     C*
202700061122     C                   EVAL      VID08 = 'In elaborazione'
202800061122     C                   SETON                                        98
202900061122     C                   WRITE     SP20D03
203000061122     C*
203100061122     C* Inizializzo il contatore d stesso sin su stesso cap
203200061122     C                   eval      w_cnt_sin = *zeros
203300061122     C*
203400061122     C/EXEC SQL
203500061122     C+ declare C11 cursor for
203600061122     C+ select cplfi1, cpscap, cpssin, cpsloc from azcps00f join azcpl00f
203700061122     C+ on  cpsver=cplver and cpsnar=cplnar and cpscap=cplcap
203800061122     C+ and cpsloc=cplloc
203900061123     C+ where cpsver=:NumVER
204000061122     C+ order by cpscap, cpssin, cplfi1
204100061122     C+ for read only
204200061122     C/END-EXEC
204300061122     C
204400061122     C/EXEC SQL
204500061122     C+ open C11
204600061122     C/END-EXEC
204700061122     C
204800061122     C/EXEC SQL
204900061122     C+ Fetch next from C11 into :ds_dup_sin
205000061122     C/END-EXEC
205100061122     C*
205200061122     C* Se reperito qualcosa => analizzo
205300061122     C                   dow       sqlcod = *zeros
205400061122     C*
205500061122     C* Verifico se già presente x tesso cap
205600061122     C                   if        d_dup_cap = cnt_sin_cap AND
205700061122     C                             d_dup_sin = cnt_sin_sin
205800061122     C*
205900061122     C* Incremento il contatore d stesso sin su stesso cap
206000061122     C                   eval      w_cnt_sin = w_cnt_sin + 1
206100061122     C*
206200061122     C* Se contatore > 1 => annullo il sin corrente
206300061122     C                   if        w_cnt_sin > 1
206400061122     C                   eval      key_sin_loc = d_dup_loc
206500061122     C                   exsr      annSIN
206600061122     C                   endif
206700061122     C*
206800061122     C* Se nn già presente salvo cap/sin correnti
206900061122     C                   else
207000061122     C*
207100061122     C* Re-imposto a 1 il contatore d sin x stesso cap
207200061122     C                   eval      w_cnt_sin = 1
207300061122     C*
207400061122     C* Quindi salvo il cap/sin corrente
207500061122     C                   eval      cnt_sin_cap = d_dup_cap
207600061122     C                   eval      cnt_sin_sin = d_dup_sin
207700061122     C                   endif
207800061122     C*
207900061122     C* Proseguo con la lettura dei sin
208000061122     C*
208100061122     C/EXEC SQL
208200061122     C+ Fetch next from C11 into :ds_dup_sin
208300061122     C/END-EXEC
208400061122     C*
208500061122     C                   enddo
208600061122     C*
208700061122     C/EXEC SQL
208800061122     C+ close C11
208900061122     C/END-EXEC
209000061122     C*
209100061122     C                   EVAL      VID08 = 'Fatto'
209200061122     C                   SETOFF                                       98
209300061122     C                   WRITE     SP20D03
209400061122     C*
209500061122     C                   ENDSR
209600061120     C*--------------------------------------------------------------------------------------------*
209700061120     C* annSIN - annulla sinonimo
209800061120     C*--------------------------------------------------------------------------------------------*
209900061120     C     annSIN        BEGSR
210000061120     C*
210100061120     C                   eval      K_cpsVER = NumVER
210200061120     C                   eval      K_cpsNAR = *blanks
210300061122     C                   eval      K_cpsCAP = d_dup_cap
210400061120     C                   eval      K_cpsLOC = key_sin_loc
210500061122     C                   eval      K_cpsSIN = d_dup_sin
210600061120     C     KEYcps01_C    chain(e)  azcps01l
210700061120     C                   if        %error
210800061120     C                   eval      wrk_error = 'E'
210900061120     C                   except    rigaSin
211000061120     C                   else
211100061120     C                   if        %found(azcps01l)
211200061120     C                   eval      cpsATB = 'A'
211300061120     C                   eval      cpsFTR = *blanks
211400080326     C                   eval      cpsDTR = *all'8'
211500061120     C                   update    azcps000
211600061120     C                   eval      wrk_error = *blanks
211700061120     C                   except    rigaSin
211800061120     C                   endif
211900061120     C                   endif
212000061120     C*
212100061120     C                   ENDSR
212200061024     C*--------------------------------------------------------------------------------------------*
212300061024     C* updFCPU - aggiornamento provincie alternative
212400061024     C*--------------------------------------------------------------------------------------------*
212500061024     C     updFCPU       BEGSR
212600061024     C*
212700061024     C                   EVAL      VID02 = 'In elaborazione'
212800061024     C                   SETON                                        92
212900061024     C                   WRITE     SP20D03
213000061024     C*
213100061024     C                   eval      wSQL = 'update azcpc00f ' +
213200061024     C                             'set cpcPRV=''FC'', cpcPRA=''FO'' ' +
213300061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''FO'' '+
213400061024     C                             'and cpcPRA=''FC'''
213500061024     C*
213600061024     C/EXEC SQL
213700061024     C+ execute immediate: wSQL
213800061024     C/END-EXEC
213900061024     C*
214000061024     C                   eval      wSQL = 'update azcpc00f ' +
214100061024     C                             'set cpcPRV=''PU'', cpcPRA=''PS'' ' +
214200061024     C                             'where cpcver='+VIDVER+' and cpcPRV=''PS'' '+
214300061024     C                             'and cpcPRA=''PU'''
214400061024     C*
214500061024     C/EXEC SQL
214600061024     C+ execute immediate: wSQL
214700061024     C/END-EXEC
214800061024     C*
214900061024     C                   ENDSR
215000061017     C*--------------------------------------------------------------------------------------------*
215100061017     C* inzpt - inizializzazione flag di elaborazione file PT
215200061017     C*--------------------------------------------------------------------------------------------*
215300061017     C     inzpt         BEGSR
215400061018     C*
215500061018     C                   EVAL      VID01 = 'In elaborazione'
215600061018     C                   SETON                                        91
215700061018     C                   WRITE     SP20D03
215800061017     C*
215900061017     C                   eval      wSQL = 'update cap_cap set FLG1 = '' '', ' +
216000061017     C                                                       'FLG2 = '' '''
216100061017     C*
216200061017     C/EXEC SQL
216300061017     C+ execute immediate: wSQL
216400061017     C/END-EXEC
216500061017     C*
216600061017     C                   eval      wSQL = 'update cap_loc set FLG1 = '' '', ' +
216700061017     C                                                       'FLG2 = '' '''
216800061017     C*
216900061017     C/EXEC SQL
217000061017     C+ execute immediate: wSQL
217100061017     C/END-EXEC
217200061017     C                   eval      wSQL = 'update cap_sin set FLG1 = '' '', ' +
217300061017     C                                                       'FLG2 = '' '''
217400061017     C*
217500061017     C/EXEC SQL
217600061017     C+ execute immediate: wSQL
217700061017     C/END-EXEC
217800061017     C*
217900061017     C                   eval      wSQL = 'delete from azcpc00f where cpcver='+
218000061017     C                                    VIDVER
218100061017     C*
218200061017     C/EXEC SQL
218300061017     C+ execute immediate: wSQL
218400061017     C/END-EXEC
218500061017     C*
218600061017     C                   eval      wSQL = 'delete from azcpl00f where cplver='+
218700061017     C                                    VIDVER
218800061017     C*
218900061017     C/EXEC SQL
219000061017     C+ execute immediate: wSQL
219100061017     C/END-EXEC
219200061017     C                   eval      wSQL = 'delete from azcps00f where cpsver='+
219300061017     C                                    VIDVER
219400061017     C*
219500061017     C/EXEC SQL
219600061017     C+ execute immediate: wSQL
219700061017     C/END-EXEC
219800140611     C                   eval      wSQL = 'delete from azcos00f where cosver='+
219900140611     C                                    VIDVER
220000140611     C*
220100140611     C/EXEC SQL
220200140611     C+ execute immediate: wSQL
220300140611     C/END-EXEC
220400061017     C*
220500061017     C                   CALL      'TISP20C1'
220600061017     C*
220700061017     C                   eval      wSQL = 'update azcpc00f set cpcFTR = ''T'' '+
220800061017     C                                    'where cpcver='+VIDVER
220900061017     C*
221000061017     C/EXEC SQL
221100061017     C+ execute immediate: wSQL
221200061017     C/END-EXEC
221300061017     C*
221400061017     C                   eval      wSQL = 'update azcpl00f set cplFTR = ''T'' '+
221500061017     C                                    'where cplver='+VIDVER
221600061017     C*
221700061017     C/EXEC SQL
221800061017     C+ execute immediate: wSQL
221900061017     C/END-EXEC
222000061017     C                   eval      wSQL = 'update azcps00f set cpsFTR = ''T'' '+
222100061017     C                                    'where cpsver='+VIDVER
222200061017     C*
222300061017     C/EXEC SQL
222400061017     C+ execute immediate: wSQL
222500061017     C/END-EXEC
222600140611     C                   eval      wSQL = 'update azcos00f set cosduv = '+
222700140611     C                                    %char(%dec(%date() : *ISO)) +
222800140611     C                                    'where cpsver='+VIDVER
222900140611     C*
223000140611     C/EXEC SQL
223100140611     C+ execute immediate: wSQL
223200140611     C/END-EXEC
223300061018     C*
223400061018     C                   EVAL      VID01 = 'Fatto'
223500061018     C                   SETOFF                                       91
223600061018     C                   WRITE     SP20D03
223700061017     C*
223800061017     C                   ENDSR
223900061024     C*--------------------------------------------------------------------------------------------*
224000061024     C* cpctocpl - passaggio dati da livello "C" a "L"
224100061024     C*--------------------------------------------------------------------------------------------*
224200061024     C     cpctocpl      BEGSR
224300061024     C*
224400061024     C                   exsr      rtvcap
224500061024     C                   if        *in52 = *on
224600061024     C                   eval      cplLNA = cpcLNA
224700061024     C                   eval      cplZNC = cpcZNC
224800061024     C                   eval      cplISO = cpcISO
224900061024     C                   eval      cplZFD = cpcZFD
225000061024     C                   eval      cplLKG = cpcLKG
225100061024     C                   eval      cplLMC = cpcLMC
225200061024     C                   eval      cplLOL = cpcLOL
225300061024     C                   eval      cplZOL = cpcZOL
225400061024     C                   eval      cplLKS = cpcLKS
225500061024     C                   eval      cplLMS = cpcLMS
225600061024     C                   eval      cplLOS = cpcLOS
225700061024     C                   eval      cplZOS = cpcZOS
225800061024     C                   eval      cplLK2 = cpcLK2
225900061024     C                   eval      cplLM2 = cpcLM2
226000061024     C                   eval      cplLO2 = cpcLO2
226100061024     C                   eval      cplZO2 = cpcZO2
226200061024     C                   eval      cplCTS = cpcCTS
226300061024     C                   eval      cplCTE = cpcCTE
226400061024     C                   eval      cplTTC = cpcTTC
226500061024     C                   eval      cplPII = cpcPII
226600061024     C                   eval      cplPID = cpcPID
226700061024     C                   eval      cplPIS = cpcPIS
226800061024     C                   eval      cplGDD = cpcGDD
226900061121     C                   eval      cplFI2 = cpcFI2
227000061024     C                   endif
227100061024     C*
227200061024     C                   ENDSR
227300000000     C*------------------------------------------------------------------------*
227400980403     C* *INZSR - OPERAZIONI INIZIALI
227500000000     C*------------------------------------------------------------------------*
227600980403     C     *INZSR        BEGSR
227700000000     C*
227800000000     C* RICEVIMENTO PARAMETRI
227900000000     C     *ENTRY        PLIST
228000000000     C                   PARM                    KPJBA
228100061017     C*
228200061017     C* Definizione chiavi di lettura
228300061017     C*
228400061017     C* AZCPC01L - Completa
228500061017     C     KEYcpc01_C    KLIST
228600061017     C                   KFLD                    K_cpcVER
228700061017     C                   KFLD                    K_cpcNAR
228800061017     C                   KFLD                    K_cpcCAP
228900061018     C*
229000061018     C* AZCPC01L - Parziale
229100061018     C     KEYcpc01_P    KLIST
229200061018     C                   KFLD                    K_cpcVER
229300061018     C                   KFLD                    K_cpcNAR
229400061017     C*
229500061017     C* AZCPL01L - Completa
229600061017     C     KEYcpl01_C    KLIST
229700061017     C                   KFLD                    K_cplVER
229800061017     C                   KFLD                    K_cplNAR
229900061017     C                   KFLD                    K_cplCAP
230000061017     C                   KFLD                    K_cplLOC
230100061017     C*
230200061017     C* AZCPS01L - Parziale
230300061017     C     KEYcps01_P    KLIST
230400061017     C                   KFLD                    K_cpsVER
230500061017     C                   KFLD                    K_cpsNAR
230600061017     C                   KFLD                    K_cpsCAP
230700061017     C                   KFLD                    K_cpsLOC
230800061120     C*
230900061120     C* AZCPS01L - Completa
231000061120     C     KEYcps01_C    KLIST
231100061120     C                   KFLD                    K_cpsVER
231200061120     C                   KFLD                    K_cpsNAR
231300061120     C                   KFLD                    K_cpsCAP
231400061120     C                   KFLD                    K_cpsLOC
231500061120     C                   KFLD                    K_cpsSIN
231600140611     C*
231700140611     C* AZCOS01L - Parziale
231800140611     C     KEYcos01_P    KLIST
231900140611     C                   KFLD                    K_cosVER
232000140611     C                   KFLD                    K_cosNAR
232100140611     C                   KFLD                    K_cosCAP
232200140611     C                   KFLD                    K_cosLOC
232300000104     C*
232400000104     C* AZZERO INIZIALE INDICATORI
232500061017     C                   SETOFF                                       40
232600000000     C*
232700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
232800000000     C     XMSGPA        PLIST
232900000000     C                   PARM                    DSMSMS
233000000000     C                   PARM                    DSMSPG
233100000000     C                   PARM                    DSMSKE
233200000000     C                   PARM                    DSMSER
233300000000     C*
233400000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
233500000000     C                   CALL      'XKEY'
233600000000     C                   PARM                    DSKEY
233700000000     C*
233800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
233900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
234000000000     C*
234100000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
234200000000     C*   -NOME PGM CHIAMANTE
234300000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
234400061017     C                   MOVEL     'TISP20R'     DSMSPG
234500061017     C                   MOVEL     'TISP20R'     MSGQUE
234600000000     C                   MOVEL     *BLANKS       MSGKEY
234700000000     C                   MOVEL     *BLANKS       DSMSMS
234800000000     C*
234900000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
235000000000     C                   Z-ADD     0             FUNGRU            5 0
235100000000     C*
235200000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
235300000000     C                   EXSR      FUNGES
235400000000     C*
235500000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
235600000000     C                   MOVEL     '0'           F12L00            1
235700000000     C                   MOVEL     '0'           F03L00            1
235800000000     C                   MOVEL     '0'           FINL00            1
235900000000     C                   MOVEL     '0'           ERRL00            1
236000000000     C                   MOVEL     '0'           FLFINE            1
236100000000     C*
236200000000     C* LANCIO ROUTINE PER VERIFICARE SE
236300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
236400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
236500020917     C                   EXSR      REPDATIUTE
236600000000     C*
236700000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
236800000000     C                   EXSR      AZZVID
236900000000     C*
237000000000     C                   ENDSR
237100000000     C*------------------------------------------------------------------------*
237200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
237300000000     C*------------------------------------------------------------------------*
237400000000     C     FUNGES        BEGSR
237500000000     C*
237600000000     C* PULISCO CAMPI DI LAVORO
237700000000     C                   Z-ADD     0             W                 5 0
237800000000     C*
237900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
238000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
238100000000     C     1             DO        24            I
238200000000     C                   MOVEL     FUD(I)        FUDRIG
238300000000     C*
238400000000     C* PULIZIA FUNZIONI PRECEDENTI
238500000000     C                   MOVEL     *BLANKS       FUV(I)
238600000000     C*
238700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
238800000000     C* (SALTA F24)
238900000000     C     FUDATT        IFEQ      'S'
239000000000     C     FUDFUN        ANDNE     '24'
239100000000     C                   ADD       1             W
239200000000     C                   MOVEL     FUDDES        FUV(W)
239300000000     C                   END
239400000000     C                   END                                                    DO WHILE
239500000000     C*
239600000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
239700000000     C* ALTRIMENTI DISATTIVO F24
239800000000     C     1             DO        24            I
239900000000     C                   MOVEL     FUD(I)        FUDRIG
240000000000     C*
240100000000     C     FUDFUN        IFEQ      '24'
240200000000     C     W             IFGT      6
240300000000     C                   MOVEL     'S'           FUDATT
240400000000     C                   ELSE
240500000000     C                   MOVEL     *BLANKS       FUDATT
240600000000     C                   END                                                    W GT 8
240700000000     C                   MOVEL     FUDRIG        FUD(I)
240800000000     C                   END
240900000000     C                   END                                                    DO WHILE
241000000000     C*
241100000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
241200000000     C     W             DIV       6             I
241300000000     C                   MVR                     Z
241400000000     C*
241500000000     C     Z             IFNE      0
241600000000     C     I             ADD       1             Z
241700000000     C                   ELSE
241800000000     C                   Z-ADD     I             Z                 5 0
241900000000     C                   END
242000000000     C*
242100000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
242200000000     C                   ADD       1             FUNGRU
242300000000     C     FUNGRU        IFGT      Z
242400000000     C     FUNGRU        ORLT      1
242500000000     C                   Z-ADD     1             FUNGRU
242600000000     C                   END
242700000000     C*
242800000000     C* PRIMO GRUPPO DI 6 FUNZIONI
242900000000     C     FUNGRU        IFEQ      1
243000000000     C                   MOVEL     FUV(1)        FUN001
243100000000     C                   MOVEL     FUV(2)        FUN002
243200000000     C                   MOVEL     FUV(3)        FUN003
243300000000     C                   MOVEL     FUV(4)        FUN004
243400000000     C                   MOVEL     FUV(5)        FUN005
243500000000     C                   MOVEL     FUV(6)        FUN006
243600000000     C                   END
243700000000     C*
243800000000     C* SECONDO GRUPPO DI 6 FUNZIONI
243900000000     C     FUNGRU        IFEQ      2
244000000000     C                   MOVEL     FUV(7)        FUN001
244100000000     C                   MOVEL     FUV(8)        FUN002
244200000000     C                   MOVEL     FUV(9)        FUN003
244300000000     C                   MOVEL     FUV(10)       FUN004
244400000000     C                   MOVEL     FUV(11)       FUN005
244500000000     C                   MOVEL     FUV(12)       FUN006
244600000000     C                   END
244700000000     C*
244800000000     C* TERZO GRUPPO DI 6 FUNZIONI
244900000000     C     FUNGRU        IFEQ      3
245000000000     C                   MOVEL     FUV(13)       FUN001
245100000000     C                   MOVEL     FUV(14)       FUN002
245200000000     C                   MOVEL     FUV(15)       FUN003
245300000000     C                   MOVEL     FUV(16)       FUN004
245400000000     C                   MOVEL     FUV(17)       FUN005
245500000000     C                   MOVEL     FUV(18)       FUN006
245600000000     C                   END
245700000000     C*
245800000000     C* QUARTO GRUPPO DI 6 FUNZIONI
245900000000     C     FUNGRU        IFEQ      4
246000000000     C                   MOVEL     FUV(19)       FUN001
246100000000     C                   MOVEL     FUV(20)       FUN002
246200000000     C                   MOVEL     FUV(21)       FUN003
246300000000     C                   MOVEL     FUV(22)       FUN004
246400000000     C                   MOVEL     *BLANKS       FUN005
246500000000     C                   MOVEL     *BLANKS       FUN006
246600000000     C                   END
246700000000     C*
246800000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
246900000000     C                   MOVEL     *BLANKS       FUN007
247000000000     C                   MOVEL     *BLANKS       FUN008
247100000000     C*
247200000000     C     1             DO        24            I
247300000000     C                   MOVEL     FUD(I)        FUDRIG
247400000000     C     FUDFUN        IFEQ      '24'
247500000000     C     FUDATT        ANDEQ     'S'
247600000000     C                   MOVEL     FUDDES        FUN008
247700000000     C                   END
247800000000     C                   END                                                    DO
247900000000     C                   ENDSR
248000000000     C*------------------------------------------------------------------------*
248100000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
248200000000     C*------------------------------------------------------------------------*
248300000000     C     FUNCON        BEGSR
248400000000     C*
248500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
248600000000     C                   EXSR      FUNVAR
248700000000     C*
248800000000     C* PULISCO CAMPI DI LAVORO
248900000000     C                   Z-ADD     0             I                 5 0
249000000000     C                   Z-ADD     0             W                 5 0
249100000000     C*
249200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
249300000000     C     FUNKEY        IFNE      *BLANKS
249400000000     C     1             DO        24            I
249500000000     C                   MOVEL     FUD(I)        FUDRIG
249600000000     C*
249700000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
249800000000     C     FUNKEY        IFEQ      FUDFUN
249900000000     C     FUDATT        ANDEQ     'S'
250000000000     C                   Z-ADD     I             W
250100000000     C                   END
250200000000     C                   END                                                    DO WHILE
250300000000     C*
250400000000     C* FUNZIONE NON ATTIVA (ERRORE)
250500000000     C     W             IFEQ      0
250600000000     C*
250700000000     C* EMISSIONE MESSAGGIO DI ERRORE
250800000000     C                   MOVEL     MSG(1)        DSMSMS
250900000000     C*
251000000000     C                   END                                                    W=0
251100000000     C                   END                                                    FUNKEY NE *BLANKS
251200000000     C*
251300000000     C* FUNZIONE ATTIVA (PROCEDI ...)
251400000000     C     W             IFNE      0
251500000000     C     FUNKEY        OREQ      *BLANKS
251600000000     C*
251700000000     C     FUNKEY        IFEQ      *BLANKS
251800000000     C                   EXSR      ENTGES
251900000000     C                   END
252000000000     C     FUNKEY        IFEQ      '03'
252100000000     C                   EXSR      F03GES
252200000000     C                   END
252300000000     C     FUNKEY        IFEQ      '06'
252400000000     C                   EXSR      F06GES
252500000000     C                   END
252600000000     C     FUNKEY        IFEQ      '12'
252700000000     C                   EXSR      F12GES
252800000000     C                   END
252900000000     C     FUNKEY        IFEQ      '24'
253000000000     C                   EXSR      F24GES
253100000000     C                   END
253200000000     C     FUNKEY        IFEQ      'HP'
253300000000     C                   EXSR      HELGES
253400000000     C                   END
253500000000     C*
253600000000     C                   END
253700000000     C*
253800000000     C                   ENDSR
253900000000     C*------------------------------------------------------------------------*
254000000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
254100000000     C*------------------------------------------------------------------------*
254200000000     C     FUNVAR        BEGSR
254300000000     C*
254400000000     C* INIZIALIZZA VARIABILE -FUNKEY-
254500000000     C*
254600000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
254700000000     C     KEY           IFEQ      ENTER
254800000000     C                   MOVEL     *BLANKS       FUNKEY
254900000000     C                   END
255000000000     C     KEY           IFEQ      F1
255100000000     C                   MOVEL     '01'          FUNKEY
255200000000     C                   END
255300000000     C     KEY           IFEQ      F2
255400000000     C                   MOVEL     '02'          FUNKEY
255500000000     C                   END
255600000000     C     KEY           IFEQ      F3
255700000000     C                   MOVEL     '03'          FUNKEY
255800000000     C                   END
255900000000     C     KEY           IFEQ      F4
256000000000     C                   MOVEL     '04'          FUNKEY
256100000000     C                   END
256200000000     C     KEY           IFEQ      F5
256300000000     C                   MOVEL     '05'          FUNKEY
256400000000     C                   END
256500991103     C     KEY           IFEQ      F6
256600000000     C                   MOVEL     '06'          FUNKEY
256700000000     C                   END
256800000000     C     KEY           IFEQ      F7
256900000000     C                   MOVEL     '07'          FUNKEY
257000000000     C                   END
257100000000     C     KEY           IFEQ      F8
257200000000     C                   MOVEL     '08'          FUNKEY
257300000000     C                   END
257400000000     C     KEY           IFEQ      F9
257500000000     C                   MOVEL     '09'          FUNKEY
257600000000     C                   END
257700000000     C     KEY           IFEQ      F10
257800000000     C                   MOVEL     '10'          FUNKEY
257900000000     C                   END
258000000000     C     KEY           IFEQ      F11
258100000000     C                   MOVEL     '11'          FUNKEY
258200000000     C                   END
258300000000     C     KEY           IFEQ      F12
258400000000     C                   MOVEL     '12'          FUNKEY
258500000000     C                   END
258600000000     C     KEY           IFEQ      F13
258700000000     C                   MOVEL     '13'          FUNKEY
258800000000     C                   END
258900000000     C     KEY           IFEQ      F14
259000000000     C                   MOVEL     '14'          FUNKEY
259100000000     C                   END
259200000000     C     KEY           IFEQ      F15
259300000000     C                   MOVEL     '15'          FUNKEY
259400000000     C                   END
259500000000     C     KEY           IFEQ      F16
259600000000     C                   MOVEL     '16'          FUNKEY
259700000000     C                   END
259800000000     C     KEY           IFEQ      F17
259900000000     C                   MOVEL     '17'          FUNKEY
260000000000     C                   END
260100000000     C     KEY           IFEQ      F18
260200000000     C                   MOVEL     '18'          FUNKEY
260300000000     C                   END
260400000000     C     KEY           IFEQ      F19
260500000000     C                   MOVEL     '19'          FUNKEY
260600000000     C                   END
260700000000     C     KEY           IFEQ      F20
260800000000     C                   MOVEL     '20'          FUNKEY
260900000000     C                   END
261000000000     C     KEY           IFEQ      F21
261100000000     C                   MOVEL     '21'          FUNKEY
261200000000     C                   END
261300000000     C     KEY           IFEQ      F22
261400000000     C                   MOVEL     '22'          FUNKEY
261500000000     C                   END
261600000000     C     KEY           IFEQ      F23
261700000000     C                   MOVEL     '23'          FUNKEY
261800000000     C                   END
261900000000     C     KEY           IFEQ      F24
262000000000     C                   MOVEL     '24'          FUNKEY
262100000000     C                   END
262200000000     C     KEY           IFEQ      ROLLUP
262300000000     C                   MOVEL     'RU'          FUNKEY
262400000000     C                   END
262500000000     C     KEY           IFEQ      HELP
262600000000     C                   MOVEL     'HP'          FUNKEY
262700000000     C                   END
262800000000     C                   ENDSR
262900000000     C*------------------------------------------------------------------------*
263000000000     C* NOTE PER COMPILAZIONE SCHIERE
263100000000     C*------------------------------------------------------------------------*
263200000000     C* -MSG-
263300000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
263400000000     C* (NON TOCCARE MSG,1)
263500000000     C*
263600000000     C* -CMD-
263700000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
263800000000     C*
263900000000     C* -FUD-
264000000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
264100000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
264200000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
264300000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
264400000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
264500000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
264600000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
264700000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
264800000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
264900000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
265000000000     C*
265100000000     C*------------------------------------------------------------------------*
265200101112     OPRTF198   E            rigaErr_T   1
265300101112     O                       wInt1
265400061017     OPRTF198   E            rigaErr     1
265500061017     O                       wRiga
265600061023     C*------------------------------------------------------------------------*
265700101112     OPRTF1982  E            rigaSI95_T  1
265800101112     O                       wInt2
265900061120     OPRTF1982  E            rigaSI95    1
266000061023     O                       wSI95
266100061120     C*------------------------------------------------------------------------*
266200101112     OQSYSPRT   E            rigaSin_T   1
266300101112     O                       wInt3
266400061120     OQSYSPRT   E            rigaSin     1
266500061122     O                       d_dup_cap           +1
266600061120     O                       key_sin_loc         +1
266700061122     O                       d_dup_sin           +1
266800061120     O                       wrk_error           +1
266900991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
267000061017TISP20R- Tasto funzionale non gestito.                                         1
267100061017TISP20R-                                                                       2
267200061017TISP20R-                                                                       3
267300061017TISP20R-                                                                       4
267400061017TISP20R- Errori nella funzione di richiesta versioni. Avvertire il CED         5
267500061017TISP20R-                                                                       6
267600061017TISP20R-                                                                       7
267700061017TISP20R- Versione Cappario BAR inesistente.                                    8
267800061017TISP20R-                                                                       9
267900061017TISP20R- Versione obbligatoria, inserire                                      10
268000061017TISP20R-                                                                      11
268100061017TISP20R-                                                                      12
268200061017TISP20R-                                                                      13
268300061017TISP20R-                                                                      14
268400061017TISP20R-                                                                      15
268500061017TISP20R-                                                                      16
268600061017TISP20R-                                                                      17
268700061017TISP20R-                                                                      18
268800061017TISP20R-                                                                      19
268900061017TISP20R-                                                                      20
269000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
269100000000                   -        (S/N)=N                        funz.01¬
269200000000                   -        (S/N)=N                        funz.02¬
269300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
269400000000                   -        (S/N)=N                        funz.04¬
269500000000                   -        (S/N)=N                        funz.05¬
269600001120F06=Esecuzione     -        (S/N)=S                        funz.06¬
269700000104                   -        (S/N)=N                        funz.07¬
269800000104F08=Ins. Versione  -        (S/N)=N                        funz.08¬
269900000000                   -        (S/N)=N                        funz.09¬
270000000000                   -        (S/N)=N                        funz.10¬
270100000000                   -        (S/N)=N                        funz.11¬
270200991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
270300000000                   -        (S/N)=N                        funz.13¬
270400000000                   -        (S/N)=N                        funz.14¬
270500000000                   -        (S/N)=N                        funz.15¬
270600000000                   -        (S/N)=N                        funz.16¬
270700000000                   -        (S/N)=N                        funz.17¬
270800000000                   -        (S/N)=N                        funz.18¬
270900000000                   -        (S/N)=N                        funz.19¬
271000000000                   -        (S/N)=N                        funz.20¬
271100000000                   -        (S/N)=N                        funz.21¬
271200000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
271300000000                   -        (S/N)=N                        funz.23¬
271400991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
