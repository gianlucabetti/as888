000100040921      /TITLE Upload via Internet: traduzione in WFTV35F (x passaggio da file a spool).
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040921     FWFTV35F   O    E             DISK    usropn
000700030225     Ftitv35p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030225     Ftitv35ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010330     D*------------------
003300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400010330     D*------------------
003500010330     D WLBDA8          DS                  INZ
003600010330     D  G08DAT                 1      8  0
003700010330     D  G08INV                 9     16  0
003800010330     D  G08ERR                17     17
003900010330     D  G08TGI                18     22  0
004000000313     D*
004100010330
004200010330
004300010330
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100030225     C                   exsr      rwtv35
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010601
005700010601
005800010601     C*--------------------------------------------------------
005900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006000010601     C*--------------------------------------------------------
006100010601     C     PREELA        BEGSR
006200010601     C*
006300010601     C* SE OCCORRE SPEDIRE IN FILIALE
006400010601     C                   if        invfil <> *zeros and
006500010601     C                             flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900000724     C*
007000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007200010601     C     invfil        chain     azorg01l
007300000724     C                   if        %found
007400000724     C                   movel(p)  CMD(1)        depcmd
007500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007600000724     C*
007700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007800011113     C                   Z-ADD     150           LENGH            15 5
007900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008000000724     C                   PARM                    depcmd
008100000724     C                   PARM                    LENGH
008200000724     C*
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000613     C*
009500030225     C                   if        not %open(titv35ps)
009600030225     C                   open      titv35ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011000010601     C*--------------------------------------------------------
011100010601     C     ENDELA        BEGSR
011200990915     C*
011300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011400000613     C*
011500030225     C                   if        %open(titv35ps)
011600000616     C                   except    findett
011700030225     C                   close     titv35ps
011800000613     C                   endif
011900000616     C*
012000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    CMD(3)
012400000616     C                   PARM                    LENGH
012500000616     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200000613
013300000613     C*--------------------------------------------------------
013400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013500000613     C*--------------------------------------------------------
013600000621     C     STPR          BEGSR
013700000613     C*
013800030225     C                   if        not %open(titv35p)
013900030225     C                   open      titv35p
014000990915     C                   endif
014100990915     C*
014200990915     C                   except    riepilogo
014300990915     C*
014400030225     C                   if        %open(titv35p)
014500030225     C                   close     titv35p
014600990914     C                   endif
014700000613     C*
014800000613     C                   ENDSR
014900000613     C***
015000000613
015100010330
015200010601
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600040921     C* RWTV35  LEGGE TIVIN00R E SCRIVE WFTV35F               *
015700010330     C*--------------------------------------------------------
015800030225     C     RWTV35        BEGSR
015900010330     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300040921     C                   if        not %open(wftv35f)
016400040921     C                   open      wftv35f
016500010330     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000010330     C*
017100010330     C                   DO        *HIVAL
017200010330     C*
017300010330     C                   READ      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900010618     C*
018000010330     C                   if        vindta > *blanks
018100010330     C                   add       1             rrnum
018200010330     C*
018300010601     C                   if        *in70 = *off and
018400010330     C                             (vinflg = *blanks
018500010330     C                              or vinflg = '0'
018600010330     C                              or vinflg = '2')
018700010330     C*
018800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018900010711     C                   if        vinflg = *blanks or vinflg = '0'
019000010711     C                   clear                   vinmsg
019100010711     C                   endif
019200010601     C*
019300010330     C                   exsr      inzvar
019400010330     C                   exsr      defcam
019500030225     C                   exsr      imptv35
019600010601     C*
019700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019800030225     C                   movel     '046'         depfil
019900010601     C                   exsr      repfil
020000010601     C                   if        depfil = invfil
020100010601     C*
020200010601     C                   exsr      PREELA
020300010601     C*
020400010604     C* Ebbene...
020500010604     C*
020600010604     C  N31              ADD       1             §CTROK            7 0
020700010604     C   32              ADD       1             §CTRMO            7 0
020800010604     C   31              ADD       1             §CTRNO            7 0
020900020205     C*
021000020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
021100040921     C  N31              WRITE     WFTV35
021200020722     C*
021300010604     C                   if        *in31 = *off and
021400010604     C                             *in32 = *off
021500010604     C                   eval      vinflg = '1'
021600010604     C                   else
021700010604     C                   eval      recko = vindta
021800010604     C                   except    rigadett
021900010604     C                   eval      vinflg = '2'
022000010604     C                   endif
022100010604     C                   endif
022200010601     C*
022300010604     C                   endif
022400010604     C*
022500010330     C                   else
022600010330     C                   eval      vinflg = '1'
022700010330     C                   endif
022800010601     C*
022900010601     C  N70              update    tivin000
023000010330     C*
023100010330     C  N70              ENDdo
023200010601     C
023300010601     C*
023400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023500010601     C                   if        cntNonEl = *zeros or
023600010601     C                             flgMulti = '0'
023700010330     C* Se non ci sono record con errori ...
023800010601     C                   if        §ctrno = 0 and
023900010604     C                             §ctrmo = 0 and
024000010601     C                             flgStato <> '2'
024100010330     C* ... restituisco esito OK.
024200010330     C                   eval      wrkesito = '0'
024300010330     C                   else
024400010330     C                   if        §ctrok > 0
024500010330     C                   eval      wrkesito = '1'
024600010330     C                   else
024700010615     C                   if        flgOk = '0'
024800010615     C                   eval      wrkesito = '2'
024900010615     C                   else
025000010615     C                   eval      wrkesito = '6'
025100010615     C                   endif
025200010330     C                   endif
025300010330     C                   endif
025400010601     C                   else
025500010601     C                   eval      wrkesito = '9'
025600010601     C                   endif
025700010330     C*
025800010330     C                   if        %open(tivin00r)
025900010330     C                   close     tivin00r
026000010330     C                   endif
026100040921     C                   if        %open(wftv35f)
026200040921     C                   close     wftv35f
026300010330     C                   endif
026400010601     C*
026500010601     C                   if        vlrpoi <> 999
026600010601     C                   eval      invfil = vlrpoi
026700010601     C                   endif
026800010330     C*
026900010330     C                   if        §ctrok > 0
027000010601     C                             and invfil > *zeros
027100010330     C                   exsr      invio
027200010330     C                   endif
027300010601     C*
027400010618     C                   if        flgGiro = '1'
027500010601     C                   exsr      endela
027600010618     C                   endif
027700010330     C*
027800010330     C                   ENDSR
027900010330     C***
028000010330
028100010601
028200010601
028300010601
028400010601
028500010330     C*----------------------------------------------------*
028600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700010330     C*----------------------------------------------------*
028800010330     C     INZVAR        BEGSR
028900010330     C*
029000010330     C*
029100010330     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029400010330     C*----------------------------------------------------*
029500010330     C     DEFCAM        BEGSR
029600010330     C*
029700020204     C* Inizializzo il buffer del record da scrivere
029800040921     C                   CLEAR                   WFTV35
029900020204     C*
030000010330     C                   ENDSR
030100010607     C*----------------------------------------------------*
030200040921     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X WFTV35F)
030300010607     C*----------------------------------------------------*
030400030225     C     IMPTV35       BEGSR
030500010607     C*
030600010607     C                   SETOFF                                       3132
030700010607     C*
030800030218     C* Valorizzo direttamente tutto il buffer del file tradotto
030900030225     C                   MOVEL(P)  vindta        tv35dta
031000020204     C*
031100010607     C                   ENDSR
031200010607     C*----------------------------------------------------*
031300020204
031400010330
031500010330
031600010601
031700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
031800010601     C     repfil        BEGSR
031900010601     C*
032000010601     C                   if        invfil = *zeros and
032100010601     C                             depfil > *zeros and
032200010629     C                             (vinflg = *blanks or
032300010629     C                              vinflg = *zeros)
032400010601     C
032500010601     C                   eval      invfil = depfil
032600010601     C                   endif
032700010601     C*
032800010601     C                   if        depfil <> invfil and
032900010601     C                             invfil > *zeros
033000010601     C                   eval      flgMulti = '1'
033100010601     C                   if        vinflg = *blanks
033200010601     C                   add       1             cntNonEl
033300010601     C                   endif
033400010601     C                   endif
033500010601     C*
033600010601     C                   if        vinflg = '2'
033700010601     C                   eval      flgStato = '2'
033800010601     C                   endif
033900010601     C*
034000010601     C                   ENDSR
034100010601     C***
034200010601
034300010330
034400010330
034500010330
034600990920      /TITLE Invio dei dati al punto operativo.
034700030225     C     invio         BEGSR
034800990920     C*
034900030225     C                   movel(p)  CMD(4)        depcmd
035000030225     C                   EVAL      depcmd=%trim(depcmd)+%trim(vlrppt)+')'
035100030225     C*
035200030225     C* OVRPRTF PER PASSAGGIO DA FILE A SPOOL
035300030225     C                   Z-ADD     150           LENGH            15 5
035400030225     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
035500030225     C                   PARM                    depcmd
035600030225     C                   PARM                    LENGH
035700990920     C                   if        %error
035800030225     C                   eval      wrkesito = '3'
035900990920     C                   endif
036000990920     C*
036100000613     C                   ENDSR
036200990910
036300010601
036400010601
036500010601
036600010601      /TITLE Invio dei dati al punto operativo.
036700010601     C     opeini        BEGSR
036800010601     C*
036900010601     C* Inizializzo flag e contatori operativi
037000010601     C                   movel     '0'           flgGiro           1
037100010601     C                   movel     '0'           flgMulti          1
037200010601     C                   movel     '1'           flgStato          1
037300010615     C                   movel     '0'           flgOk             1
037400010601     C                   z-add     *zeros        cntNonEl         10 0
037500010601     C                   z-add     *zeros        depfil            3 0
037600010601     C                   z-add     *zeros        invfil            3 0
037700010601     C*
037800010601     C                   ENDSR
037900010601     C***
038000010601
038100010601
038200010330
038300010330
038400000613     C     *inzsr        BEGSR
038500990910     C*
038600990910     C     *entry        plist
038700990920     C                   parm                    tivlrds
038800990921     C                   parm      wrkesito      esito
038900000724     C                   parm                    prmlit
039000000710     C                   parm                    prmfir
039100010330     C*
039200010330     C* CALCOLA LA DATA CORRENTE
039300010330     C                   time                    wn14             14 0
039400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
039500010330     C                   z-add     wn8           g08dat
039600010330     C                   z-add     *zeros        g08inv
039700010330     C                   movel     '0'           g08err
039800010330     C                   call      'XSRDA8'
039900010330     C                   parm                    wlbda8
040000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040100000613     C*
040200000613     C                   ENDSR
040300000613     C***
040400990908
040500030225     Otitv35p   E            riepilogo         2
040600990915     O                                              'Upload via Internet'
040700990915     O                                           +1 'Traduzione TIVIN00R -'
040800040921     O                                           +1 'WFTV35F'
040900990915     O                                           +1 'Testate bolle'
041000990915     O          E            riepilogo   2
041100990915     O                       wrkdata
041200990915     O                       wrkora              +1
041300990915     O                       procname            +1
041400990915     O          E            riepilogo   2
041500990915     O                                              'Cliente..................:'
041600030218     O                       vlrksc              +1
041700990915     O          E            riepilogo   2
041800990920     O                                              'Riferimento Strategi.....:'
041900990920     O                       vlrhdl              +1
042000990915     O          E            riepilogo   2
042100000613
042200030225     Otitv35ps  E            testdett          2
042300000613     O                                              'Upload via Internet'
042400000613     O                                           +1 'Traduzione TIVIN00R -'
042500040921     O                                           +1 'WFTV35F'
042600000613     O                                           +1 'Testate bolle'
042700000616     O          E            testdett    3
042800000613     O                                           +2 'N° rec'
042900000613     O                                           +3 'Anteprima contenuto'
043000000616     O          E            rigadett    2
043100000613     O                       rrnum               +2
043200000621     O                       recko               +3
043300000616     O          E            findett     2
043400000613     O                       wrkdata
043500000613     O                       wrkora              +1
043600000613     O                       procname            +1
043700000616     O          E            findett     2
043800000613     O                                              'Cliente..................:'
043900030218     O                       vlrksc              +1
044000000616     O          E            findett     2
044100000613     O                                              'Riferimento Strategi.....:'
044200000613     O                       vlrhdl              +1
044300000616     O          E            findett     2
044400000613** CMD - COMANDI CL
044500030225OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV35P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
044600030225OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV35PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
044700030225DLTOVR FILE(TITV35P TITV35PS) LVL(*)
044800040921CPYF FROMFILE(WFTV35F) TOFILE(
044900000613
