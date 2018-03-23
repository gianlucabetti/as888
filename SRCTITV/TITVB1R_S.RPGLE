000100060626      /TITLE Upload via Internet: traduzione in FIVABWWR. (x STANHOME Cod. 0632875)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030130     FFIVABwwr  O    E             DISK    usropn
000700060626     FFIVATwwr  O    E             DISK    usropn
000800040405     Ftitvb1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000040405     Ftitvb1ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300060626     D parccm          s              8    INZ(*blanks)
003400060626     D parmbr          s             10    INZ(*blanks)
003500060626     D paropz          s              1    INZ(*blanks)
003600060626     D chkcall         s              1    INZ(*blanks)
003700010330
003800010330
003900010330
004000990915     C                   time                    wrkdata
004100990915     C                   time                    wrkora
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500010601     C*
004600010601     C                   exsr      opeini
004700010605     C                   exsr      rwvab
004800060626     C*
004900060626     C* Effettuo la chiamata al CLLE preposto
005000060626     C                   call(e)   'TITVVTC'
005100060626     C                   parm                    parccm
005200060626     C                   parm                    parmbr
005300060626     C                   parm      '2'           paropz
005400010601     C*
005500010601     C                   seton                                        lr
005600010601
005700010601
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500010601     C* SE OCCORRE SPEDIRE IN FILIALE
006600010601     C                   if        invfil <> *zeros and
006700010601     C                             flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400010601     C     invfil        chain     azorg01l
007500000724     C                   if        %found
007600000724     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000724     C*
007900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000724     C                   PARM                    depcmd
008300000724     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000613     C*
009700040405     C                   if        not %open(titvb1ps)
009800040405     C                   open      titvb1ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011100010601     C*--------------------------------------------------------
011200010601     C     ENDELA        BEGSR
011300990915     C*
011400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011500000613     C*
011600040405     C                   if        %open(titvb1ps)
011700000616     C                   except    findett
011800040405     C                   close     titvb1ps
011900000613     C                   endif
012000000616     C*
012100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    CMD(3)
012500000616     C                   PARM                    LENGH
012600000616     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700040405     C                   if        not %open(titvb1p)
013800040405     C                   open      titvb1p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300040405     C                   if        %open(titvb1p)
014400040405     C                   close     titvb1p
014500990914     C                   endif
014600000613     C*
014700000613     C                   ENDSR
014800000613     C***
014900000613
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300030130     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000030130     C                   if        not %open(fivabwwr)
016100030130     C                   open      fivabwwr
016200010330     C                   endif
016300060626     C*
016400060626     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500060626     C                   exsr      prevat
016600060626     C*
016700060626     C                   if        chkcall = '0'
016800060626     C*
016900060626     C                   if        not %open(fivatwwr)
017000060626     C                   open      fivatwwr
017100060626     C                   endif
017200010330     C*
017300060626     C                   clear                   §CTROKVB          5 0
017400060626     C                   clear                   §CTROKVT          5 0
017500010604     C                   clear                   §CTRMO
017600010604     C                   clear                   §CTRNO
017700010330     C*
017800010330     C                   DO        *HIVAL
017900010330     C*
018000010330     C                   READ      tivin00r                               70
018100010618     C*
018200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018300010618     C                   if        vinflg = '1'
018400010618     C                   eval      flgOk = '1'
018500010618     C                   endif
018600010618     C*
018700010330     C                   if        vindta > *blanks
018800010330     C                   add       1             rrnum
018900010330     C*
019000010601     C                   if        *in70 = *off and
019100010330     C                             (vinflg = *blanks
019200010330     C                              or vinflg = '0'
019300010330     C                              or vinflg = '2')
019400010330     C*
019500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019600010711     C                   if        vinflg = *blanks or vinflg = '0'
019700010711     C                   clear                   vinmsg
019800010711     C                   endif
019900010601     C*
020000010330     C                   exsr      inzvar
020100010330     C                   exsr      defcam
020200010605     C                   exsr      impvab
020300010601     C*
020400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020500010605     C                   eval      depfil = VABLNP
020600010601     C                   exsr      repfil
020700010601     C                   if        depfil = invfil
020800030130     C                   if        vlrpoi = 999
020900030130     C                   MOVE(P)   invfil        VABFGS
021000030130     C                   else
021100030130     C                   MOVE(P)   vlrpoi        VABFGS
021200030130     C                   endif
021300010601     C*
021400010601     C                   exsr      PREELA
021500010601     C*
021600010604     C* Ebbene...
021700010604     C*
021800060626     C  N31              ADD       1             §CTROKVB
021900010604     C   32              ADD       1             §CTRMO            7 0
022000010604     C   31              ADD       1             §CTRNO            7 0
022100020205     C*
022200060626     C  N31              exsr      wrivat                                       => carico/scrivo VAT
022300030130     C  N31              WRITE     FIVAB000
022400020722     C*
022500010604     C                   if        *in31 = *off and
022600010604     C                             *in32 = *off
022700010604     C                   eval      vinflg = '1'
022800010604     C                   else
022900010604     C                   eval      recko = vindta
023000010604     C                   except    rigadett
023100010604     C                   eval      vinflg = '2'
023200010604     C                   endif
023300010604     C                   endif
023400010601     C*
023500010604     C                   endif
023600010604     C*
023700010330     C                   else
023800010330     C                   eval      vinflg = '1'
023900010330     C                   endif
024000010601     C*
024100010601     C  N70              update    tivin000
024200010330     C*
024300010330     C  N70              ENDdo
024400060626     C*
024500060626     C                   endif                                                  <= end PREVAT
024600010601     C*
024700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024800010601     C                   if        cntNonEl = *zeros or
024900010601     C                             flgMulti = '0'
025000010330     C* Se non ci sono record con errori ...
025100010601     C                   if        §ctrno = 0 and
025200010604     C                             §ctrmo = 0 and
025300010601     C                             flgStato <> '2'
025400010330     C* ... restituisco esito OK.
025500010330     C                   eval      wrkesito = '0'
025600010330     C                   else
025700060626     C                   if        §ctrokvb > 0
025800010330     C                   eval      wrkesito = '1'
025900010330     C                   else
026000010615     C                   if        flgOk = '0'
026100010615     C                   eval      wrkesito = '2'
026200010615     C                   else
026300010615     C                   eval      wrkesito = '6'
026400010615     C                   endif
026500010330     C                   endif
026600010330     C                   endif
026700010601     C                   else
026800010601     C                   eval      wrkesito = '9'
026900010601     C                   endif
027000010330     C*
027100010330     C                   if        %open(tivin00r)
027200010330     C                   close     tivin00r
027300010330     C                   endif
027400030130     C                   if        %open(fivabwwr)
027500030130     C                   close     fivabwwr
027600010330     C                   endif
027700060626     C                   if        %open(fivatwwr)
027800060626     C                   close     fivatwwr
027900060626     C                   endif
028000010601     C*
028100010601     C                   if        vlrpoi <> 999
028200010601     C                   eval      invfil = vlrpoi
028300010601     C                   endif
028400010330     C*
028500060626     C                   if        §ctrokvb > 0
028600010601     C                             and invfil > *zeros
028700010330     C                   exsr      invio
028800010330     C                   endif
028900010601     C*
029000010618     C                   if        flgGiro = '1'
029100010601     C                   exsr      endela
029200010618     C                   endif
029300010330     C*
029400010330     C                   ENDSR
029500010330     C***
029600010330
029700010601
029800060626     C*----------------------------------------------------*
029900060626     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
030000060626     C*----------------------------------------------------*
030100060626     C     WRIVAT        BEGSR
030200070314     C*
030300070314     C                   eval      VATAAS = VABAAS
030400070314     C                   eval      VATLNP = VABLNP
030500070314     C                   eval      VATNRS = VABNRS
030600070314     C                   eval      VATNSP = VABNSP
030700070314     C                   eval      VATCCM = VABCCM
030800060626     C*
030900060626     C* Se richiesta gestione Disk C nei parametri
031000060626     C                   if        wDISK = 'C'
031100060626     C*
031200060626     C* X ogni collo indicato in testata
031300060626     C                   z-add     1             wColli            5 0
031400060626     C                   dow       wColli <= VABNCL
031500060626     C*
031600060626     C                   eval      VATTRC = 'E'
031700060626     C*
031800060626     C* Valorizzo il buffer di scrittura del FIVAT
031900080923     C                   move(p)   wColli        wColliAlfa        3
032000060626     C                   eval      VATNOT = %subst(vindta:79:7)+
032100060626     C                                      wColliAlfa
032200060626     C                   write     FIVAT000
032300060626     C*
032400060626     C                   add       1             wColli
032500060626     C                   enddo
032600060626     C*
032700060626     C                   endif
032800070314     C*
032900070314     C* Imposto il tipo record B x il numero telefono destinatario
033000070314     C                   eval      VATTRC = 'B'
033100070314     C                   eval      VATNOT = %trim(%subst(vindta:147:5))+' '+
033200100714     C                                      %trim(%subst(vindta:152:9))
033300070314     C                   if        VATNOT <> *blanks
033400070314     C                   write     FIVAT000
033500070314     C                   endif
033600060626     C*
033700060626     C                   ENDSR
033800060626     C*----------------------------------------------------*
033900060626     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
034000060626     C*----------------------------------------------------*
034100060626     C     PREVAT        BEGSR
034200060626     C*
034300060626     C* Compongo il nome del membro da dare al FIVATWWR
034400060626     C                   eval      parmbr = vlrhdl
034500060626     C                   movel     'M'           parmbr
034600060626     C                   eval      parccm = vlrksc
034700060626     C                   eval      paropz = '1'
034800060626     C* Effettuo la chiamata al CLLE preposto
034900060626     C                   call(e)   'TITVVTC'
035000060626     C                   parm                    parccm
035100060626     C                   parm                    parmbr
035200060626     C                   parm                    paropz
035300060626     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
035400060626     C                   if        %error
035500060626     C                   movel     '1'           chkcall
035600060626     C                   else
035700060626     C                   movel     '0'           chkcall
035800060626     C                   endif
035900060626     C*
036000060626     C                   ENDSR
036100010601
036200010601
036300010601
036400010330     C*----------------------------------------------------*
036500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036600010330     C*----------------------------------------------------*
036700010330     C     INZVAR        BEGSR
036800010330     C*
036900020204     C                   Z-ADD     *zeros        Num5_0            5 0
037000060626     C                   Z-ADD     *zeros        Num7_0            7 0
037100010330     C*
037200010330     C                   ENDSR
037300010330     C*----------------------------------------------------*
037400020722     C*  IMPOSTAZIONE CAMPI COSTANTI
037500010330     C*----------------------------------------------------*
037600010330     C     DEFCAM        BEGSR
037700010330     C*
037800020204     C* Inizializzo il buffer del record da scrivere
037900030130     C                   CLEAR                   FIVAB000
038000020204     C* Imposto i valori di default...
038100060626     C                   EVAL      VABCCM = 0632875
038200060626     C                   EVAL      VATCCM = 0632875
038300060626     C                   EVAL      VABLNP = 063
038400060626     C                   EVAL      VATLNP = 063
038500060626     C                   EVAL      VABCTR = 001
038600060626     C                   Z-ADD     vlrpoi        VABFGS
038700060626     C                   Z-ADD     vlrpoi        VATFGS
038800020204     C* ... e poi verifico se sono stati passati come parametri
038900020204     C                   IF        vlrppt > *blanks
039000040405     C                   IF        %subst(vlrppt:1:7) <> *blanks
039100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039200020204     C                   EXSR      CHKNUM
039300020204     C                   IF        PiInt=*on
039400020204     C                   Z-ADD     PiVal         VABCCM
039500060626     C                   Z-ADD     PiVal         VATCCM
039600020204     C                   ENDIF
039700040405     C                   ENDIF
039800040405     C                   IF        %subst(vlrppt:8:3) <> *blanks
039900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040000020204     C                   EXSR      CHKNUM
040100020204     C                   IF        PiInt=*on
040200020204     C                   Z-ADD     PiVal         VABLNP
040300060626     C                   Z-ADD     PiVal         VATLNP
040400020204     C                   ENDIF
040500040405     C                   ENDIF
040600040405     C                   IF        %subst(vlrppt:11:3) <> *blanks
040700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040800020204     C                   EXSR      CHKNUM
040900020204     C                   IF        PiInt=*on
041000020204     C                   Z-ADD     PiVal         VABCTR
041100020204     C                   ENDIF
041200040405     C                   ENDIF
041300060626     C                   IF        %subst(vlrppt:14:1) = 'C'                    * Disk C
041400060626     C                   MOVEL     'C'           wDISK             1
041500060626     C                   EVAL      VABCTM = '7Q'
041600060626     C                   ENDIF
041700071009     C                   IF        %subst(vlrppt:15:1) <> *blanks               * Consegne ai piani
041800071009     C                   EVAL      VABTC1 = %subst(vlrppt:15:1)
041900071009     C                   ENDIF
042000020204     C                   ENDIF
042100020204     C*
042200010330     C                   ENDSR
042300010607     C*----------------------------------------------------*
042400030130     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042500010607     C*----------------------------------------------------*
042600010607     C     IMPVAB        BEGSR
042700010607     C*
042800010607     C                   SETOFF                                       3132
042900030327     C*
043000030327     C                   MOVEL     datcor        VABAAS
043100030327     C                   MOVE      datcor        VABMGS
043200030327     C*
043300040405     C                   EVAL      VABRSD=%trim(%subst(vindta:8:24))
043400030327     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043500030327     C     '@':'A'       XLATE     VABRSD        VABRSD
043600030327     C* ==
043700070314     C                   EVAL      VABIND=%trim(%subst(vindta:32:25))
043800040405     C                   EVAL      VABLOD=%trim(%subst(vindta:62:15))
043900040405     C                   EVAL      VABPRD=%trim(%subst(vindta:77:2))
044000040405     C                   EVAL      VABRMA=%trim(%subst(vindta:79:7))
044100040405     C                   EVAL      VABCBO=%trim(%subst(vindta:95:1))
044200070314     C                   EVAL      VABNT2 = %trim(%subst(vindta:162:5))+' '+
044300070314     C                                      %trim(%subst(vindta:167:10))
044400100714     C                   EVAL      VABTC2 = %subst(vindta:161:1)
044500030327     C* CAP
044600040405     C                   EVAL      PiStr=%trim(%subst(vindta:57:5))
044700030327     C                   EXSR      CHKNUM
044800030327     C                   IF        PiInt=*on
044900030327     C                   Z-ADD     PiVal         Num5_0
045000030327     C                   MOVEL     Num5_0        VABCAD
045100030327     C                   ELSE
045200030327     C                   SETON                                        32
045300030327     C                   EVAL      vinmsg = %trimr(vinmsg)
045400030327     C                             + ' ' + 'VABCAD'
045500030327     C                   ENDIF
045600030327     C* NSP/RMN
045700040405     C                   EVAL      PiStr=%trim(%subst(vindta:79:7))
045800030327     C                   EXSR      CHKNUM
045900030327     C                   IF        PiInt=*on
046000030327     C                   Z-ADD     PiVal         VABRMN
046100030327     C                   Z-ADD     PiVal         Num7_0
046200030327     C                   MOVE(P)   Num7_0        VABNSP
046300030327     C                   ELSE
046400030327     C                   SETON                                        31
046500030327     C                   EVAL      vinmsg = %trimr(vinmsg)
046600030327     C                             + ' ' + 'VABNSP VABRMN'
046700030327     C                   ENDIF
046800030327     C* NCL
046900040405     C                   EVAL      PiStr=%trim(%subst(vindta:103:3))
047000030327     C                   EXSR      CHKNUM
047100030327     C                   IF        PiInt=*on
047200030327     C                   Z-ADD     PiVal         VABNCL
047300030327     C                   ELSE
047400030327     C                   SETON                                        32
047500030327     C                   EVAL      vinmsg = %trimr(vinmsg)
047600030327     C                             + ' ' + 'VABNCL'
047700030327     C                   ENDIF
047800030327     C* PKB
047900040405     C                   EVAL      PiStr=%trim(%subst(vindta:106:7))
048000030327     C                   EXSR      CHKNUM
048100040405     C                   IF        PiNum=*on
048200040405     C                   EVAL(H)   VABPKB = PIVAL/1000                          * da gr. a kg.
048300030327     C                   ELSE
048400030327     C                   SETON                                        32
048500030327     C                   EVAL      vinmsg = %trimr(vinmsg)
048600030327     C                             + ' ' + 'VABPKB'
048700030327     C                   ENDIF
048800030327     C*
048900030327     C* Considerazioni sul contenuto di campi precedentemente valorizzati
049000030327     C                   IF        VABCAS > *zeros
049100040405     C                   IF        VABCBO = '1'
049200030327     C                   EVAL      VABCBO = '4'
049300040405     C                   ENDIF
049400040405     C                   IF        VABCBO = '2'
049500040405     C                   EVAL      VABCBO = '6'
049600040405     C                   ENDIF
049700030327     C                   ENDIF
049800060908     C*
049900060908     C* Considerazioni particolari in caso d porto assegnato
050000060908     C                   IF        VABCBO = '2' OR
050100060908     C                             VABCBO = '6'
050200060908     C                   EVAL      VABFTM = 'S'
050300060908     C                   ENDIF
050400030327     C*
050500030327     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050600030327     C                   EXSR      CHKIMPDIV
050700020204     C*
050800010607     C                   ENDSR
050900010607     C*----------------------------------------------------*
051000010601
051100020204
051200020204     C*----------------------------------------------------*
051300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051400020204     C*----------------------------------------------------*
051500020204     C     CHKIMPDIV     BEGSR
051600020204     C*
051700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051800020204     C                   Z-ADD     *zeros        wrkDec            9 9
051900020204     C*
052000020204     C* Come prima cosa effettuo considerazioni sulla divisa
052100020204     C                   IF        vabIAS > *zeros
052200020204     C                   IF        vabVAS <> 'EUR'
052300020204     C                   EVAL      vabVAS =  'ITL'
052400020204     C                   ENDIF
052500020204     C                   ENDIF
052600020204     C*
052700020204     C                   IF        vabCAS > *zeros
052800020204     C                   IF        vabVCA <> 'EUR'
052900020204     C                   EVAL      vabVCA =  'ITL'
053000020204     C                   ENDIF
053100020204     C                   ENDIF
053200020204     C*
053300020204     C                   IF        vabVMD > *zeros
053400020321     C                   IF        vabVAD <> 'EUR'
053500020204     C                   EVAL      vabVAD =  'ITL'
053600020204     C                   ENDIF
053700020204     C                   ENDIF
053800020204     C*
053900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054000020204     C                   Z-ADD     vabIAS        wrkDec
054100020204     C                   IF        wrkDec > *zeros
054200020204     C                   IF        vabVAS = 'ITL'
054300020204     C                   EVAL      vabIAS = *zeros
054400020204     C                   ENDIF
054500020204     C                   ENDIF
054600020204     C*
054700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
054800020204     C                   Z-ADD     vabCAS        wrkDec
054900020204     C                   IF        wrkDec > *zeros
055000020204     C                   IF        vabVCA = 'ITL'
055100020204     C                   EVAL      vabCAS = *zeros
055200020204     C                   ENDIF
055300020204     C                   ENDIF
055400020204     C*
055500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
055600020204     C                   Z-ADD     vabVMD        wrkDec
055700020204     C                   IF        wrkDec > *zeros
055800020204     C                   IF        vabVAD = 'ITL'
055900020204     C                   EVAL      vabVMD = *zeros
056000020204     C                   ENDIF
056100020204     C                   ENDIF
056200020204     C*
056300020204     C                   ENDSR
056400020204     C***
056500020204
056600010330
056700010330
056800010330     C*----------------------------------------------------*
056900010330     C*  CONTROLLO NUMERICITA' CAMPI
057000010330     C*----------------------------------------------------*
057100010330     C     CHKNUM        BEGSR
057200010330     C*
057300010606     C                   IF        PiDecChr = *blanks
057400030327     C                   EVAL      PiDecChr = ','
057500010606     C                   ENDIF
057600010606     C*
057700010606     C                   CALL(e)   'ISNUMERIC'
057800010330     C                   PARM                    PiStr            30
057900010606     C                   PARM                    PiDecChr          1
058000010330     C                   PARM      *ZEROS        PiVal            30 9
058100010330     C                   PARM      '0'           PiInt             1
058200010330     C                   PARM      '0'           PiNum             1
058300010330     C                   IF        %error
058400030327     C                   EVAL      PiInt=*off
058500010606     C                   EVAL      PiNum=*off
058600010330     C                   ENDIF
058700010330     C*
058800010330     C                   ENDSR
058900010330     C***
059000010330
059100010601
059200010601
059300010601
059400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
059500010601     C     repfil        BEGSR
059600010601     C*
059700010601     C                   if        invfil = *zeros and
059800010601     C                             depfil > *zeros and
059900010629     C                             (vinflg = *blanks or
060000010629     C                              vinflg = *zeros)
060100010601     C
060200010601     C                   eval      invfil = depfil
060300010601     C                   endif
060400010601     C*
060500010601     C                   if        depfil <> invfil and
060600010601     C                             invfil > *zeros
060700010601     C                   eval      flgMulti = '1'
060800010601     C                   if        vinflg = *blanks
060900010601     C                   add       1             cntNonEl
061000010601     C                   endif
061100010601     C                   endif
061200010601     C*
061300010601     C                   if        vinflg = '2'
061400010601     C                   eval      flgStato = '2'
061500010601     C                   endif
061600010601     C*
061700010601     C                   ENDSR
061800010601     C***
061900010601
062000060626
062100060626
062200060626      /TITLE Invio dei dati al punto operativo.
062300060626     C     invio         BEGSR
062400060626     C*
062500060626     C* 1° invio FIVAT
062600060626     C                   reset                   dscmz
062700060626     C                   move      vlrpoi        cmzdst
062800060626     C                   eval      cmzfld = 'FIVATWWR'
062900060626     C                   eval      cmzmbd = vlrhdl
063000060626     C                   eval      %subst(cmzmbd:1:1) = 'M'
063100060626     C***                if        prmfir = *blanks
063200060626     C                   eval      cmzfla = 'FIVAT00F'
063300060626     C                   eval      cmzmba = 'FIVAT00F'
063400060626     C***                else
063500060626     C***                eval      cmzfla = prmfir
063600060626     C***                eval      cmzmba = prmfir
063700060626     C***                endif
063800060626     C                   eval      cmznrr = *zeros
063900060626     C                   move      §ctrokvt      cmznrr
064000060626     C                   eval      cmzlba = vlrfl1
064100060626     C                   call(e)   'TIS711C'
064200060626     C                   parm                    dscmz
064300060626     C                   parm      *blanks       esito
064400060626     C                   if        %error
064500060626     C                             or cmzerr = '1'
064600060626     C                             or esito  = '1'
064700060626     C                   eval      wrkesito = '3'
064800060626     C                   else
064900060626     C*
065000060626     C* 2° invio FIVAB
065100060626     C                   reset                   dscmz
065200060626     C                   move      vlrpoi        cmzdst
065300060626     C                   eval      cmzfld = vlrfou
065400060626     C                   eval      cmzmbd = vlrhdl
065500060626     C                   eval      %subst(cmzmbd:1:1) = 'M'
065600060626     C***                if        prmfir = *blanks
065700060626     C                   eval      cmzfla = 'FIVAB00F'
065800060626     C                   eval      cmzmba = 'FIVAB00F'
065900060626     C***                else
066000060626     C***                eval      cmzfla = prmfir
066100060626     C***                eval      cmzmba = prmfir
066200060626     C***                endif
066300060626     C                   eval      cmznrr = *zeros
066400060626     C                   move      §ctrokvb      cmznrr
066500060626     C                   eval      cmzlba = vlrfl1
066600060626     C                   call(e)   'TIS711C'
066700060626     C                   parm                    dscmz
066800060626     C                   parm      *blanks       esito
066900060626     C                   if        %error
067000060626     C                             or cmzerr = '1'
067100060626     C                             or esito  = '1'
067200060626     C                   eval      wrkesito = '3'
067300060626     C                   endif
067400060626     C                   endif
067500060626     C*
067600060626     C                   ENDSR
067700060626     C***
067800010601
067900060626
068000010601
068100010601      /TITLE Invio dei dati al punto operativo.
068200010601     C     opeini        BEGSR
068300010601     C*
068400010601     C* Inizializzo flag e contatori operativi
068500010601     C                   movel     '0'           flgGiro           1
068600010601     C                   movel     '0'           flgMulti          1
068700010601     C                   movel     '1'           flgStato          1
068800010615     C                   movel     '0'           flgOk             1
068900010601     C                   z-add     *zeros        cntNonEl         10 0
069000010601     C                   z-add     *zeros        depfil            3 0
069100010601     C                   z-add     *zeros        invfil            3 0
069200010601     C*
069300010601     C                   ENDSR
069400010601     C***
069500010601
069600010601
069700010601
069800010330
069900010330
070000000613     C     *inzsr        BEGSR
070100990910     C*
070200990910     C     *entry        plist
070300990920     C                   parm                    tivlrds
070400990921     C                   parm      wrkesito      esito
070500000724     C                   parm                    prmlit
070600000710     C                   parm                    prmfir
070700010330     C*
070800010330     C* CALCOLA LA DATA CORRENTE
070900100714     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071000100714     C                   eval      datcor = %dec(%date() : *ISO)
071100000613     C*
071200000613     C                   ENDSR
071300000613     C***
071400990908
071500040405     Otitvb1p   E            riepilogo         2
071600990915     O                                              'Upload via Internet'
071700990915     O                                           +1 'Traduzione TIVIN00R -'
071800030130     O                                           +1 'FIVABWWR'
071900990915     O                                           +1 'Testate bolle'
072000990915     O          E            riepilogo   2
072100990915     O                       wrkdata
072200990915     O                       wrkora              +1
072300990915     O                       procname            +1
072400990915     O          E            riepilogo   2
072500990915     O                                              'Cliente..................:'
072600010605     O                       VABCCM        z     +1
072700990915     O          E            riepilogo   2
072800990920     O                                              'Riferimento Strategi.....:'
072900990920     O                       vlrhdl              +1
073000990915     O          E            riepilogo   2
073100990915     O                                              'Giusti...................:'
073200060626     O                       §CTROKVB      2   +  1
073300990915     O          E            riepilogo   2
073400990915     O                                              'Sbagliati e corretti.....:'
073500971022     O                       §CTRMO        2   +  1
073600990915     O          E            riepilogo   2
073700990915     O                                              'Sbagliati e scartati.....:'
073800971022     O                       §CTRNO        2   +  1
073900000613
074000040405     Otitvb1ps  E            testdett          2
074100000613     O                                              'Upload via Internet'
074200000613     O                                           +1 'Traduzione TIVIN00R -'
074300030130     O                                           +1 'FIVABWWR'
074400000613     O                                           +1 'Testate bolle'
074500000616     O          E            testdett    3
074600000613     O                                           +2 'N° rec'
074700000613     O                                           +3 'Anteprima contenuto'
074800000616     O          E            rigadett    2
074900000613     O                       rrnum               +2
075000000621     O                       recko               +3
075100000616     O          E            findett     2
075200000613     O                       wrkdata
075300000613     O                       wrkora              +1
075400000613     O                       procname            +1
075500000616     O          E            findett     2
075600000613     O                                              'Cliente..................:'
075700010605     O                       VABCCM        z     +1
075800000616     O          E            findett     2
075900000613     O                                              'Riferimento Strategi.....:'
076000000613     O                       vlrhdl              +1
076100000616     O          E            findett     2
076200000613     O                                              'Giusti...................:'
076300060626     O                       §CTROKVB      2   +  1
076400000616     O          E            findett     2
076500000613     O                                              'Sbagliati e corretti.....:'
076600000613     O                       §CTRMO        2   +  1
076700000616     O          E            findett     2
076800000613     O                                              'Sbagliati e scartati.....:'
076900000613     O                       §CTRNO        2   +  1
077000000613** CMD - COMANDI CL
077100040405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
077200040405OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVB1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
077300040405DLTOVR FILE(TITVB1P TITVB1PS) LVL(*)
077400010606
077500000613
