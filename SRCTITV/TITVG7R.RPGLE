000100050114      /TITLE Upload via Internet: traduzione in FIVABWWR. (x KODAK Cod. 0893012)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021014     FFIVABwwr  O    E             DISK    usropn
000700050114     Ftitvg7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050114     Ftitvg7ps  O    f  198        PRINTER usropn
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
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010605     C                   exsr      rwvab
005100010601     C*
005200010601     C                   seton                                        lr
005300010601
005400010601
005500010601
005600010601
005700010601     C*--------------------------------------------------------
005800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005900010601     C*--------------------------------------------------------
006000010601     C     PREELA        BEGSR
006100010601     C*
006200010601     C* SE OCCORRE SPEDIRE IN FILIALE
006300010601     C                   if        invfil <> *zeros and
006400010601     C                             flgGiro = '0'
006500010601     C*
006600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006700010601     C                   eval      flgGiro = '1'
006800000724     C*
006900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100010601     C     invfil        chain     azorg01l
007200000724     C                   if        %found
007300000724     C                   movel(p)  CMD(1)        depcmd
007400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007500000724     C*
007600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011113     C                   Z-ADD     150           LENGH            15 5
007800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000724     C                   PARM                    depcmd
008000000724     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000616     C*
008400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500000616     C                   movel(p)  CMD(2)        depcmd
008600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000613     C*
009400050114     C                   if        not %open(titvg7ps)
009500050114     C                   open      titvg7ps
009600000616     C                   except    testdett
009700000613     C                   endif
009800010601     C*
009900010601     C                   endif
010000010601     C*
010100010601     C                   ENDSR
010200010601     C***
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010601     C*--------------------------------------------------------
010900010601     C     ENDELA        BEGSR
011000990915     C*
011100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011200000613     C*
011300050114     C                   if        %open(titvg7ps)
011400000616     C                   except    findett
011500050114     C                   close     titvg7ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300000616     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800000613
012900000613     C*--------------------------------------------------------
013000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013100000613     C*--------------------------------------------------------
013200000621     C     STPR          BEGSR
013300000613     C*
013400050114     C                   if        not %open(titvg7p)
013500050114     C                   open      titvg7p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000050114     C                   if        %open(titvg7p)
014100050114     C                   close     titvg7p
014200990914     C                   endif
014300000613     C*
014400000613     C                   ENDSR
014500000613     C***
014600000613
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700021014     C                   if        not %open(fivabwwr)
015800021014     C                   open      fivabwwr
015900010330     C                   endif
016000010330     C*
016100010604     C                   clear                   §CTROK
016200010604     C                   clear                   §CTRMO
016300010604     C                   clear                   §CTRNO
016400010330     C*
016500010330     C                   DO        *HIVAL
016600010330     C*
016700010330     C                   READ      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300010618     C*
017400010330     C                   if        vindta > *blanks
017500010330     C                   add       1             rrnum
017600010330     C*
017700010601     C                   if        *in70 = *off and
017800010330     C                             (vinflg = *blanks
017900010330     C                              or vinflg = '0'
018000010330     C                              or vinflg = '2')
018100010330     C*
018200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018300010711     C                   if        vinflg = *blanks or vinflg = '0'
018400010711     C                   clear                   vinmsg
018500010711     C                   endif
018600010601     C*
018700010330     C                   exsr      inzvar
018800010330     C                   exsr      defcam
018900010605     C                   exsr      impvab
019000010601     C*
019100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019200010605     C                   eval      depfil = VABLNP
019300010601     C                   exsr      repfil
019400010601     C                   if        depfil = invfil
019500021025     C                   if        vlrpoi = 999
019600021025     C                   MOVE(P)   invfil        VABFGS
019700021025     C                   else
019800021025     C                   MOVE(P)   vlrpoi        VABFGS
019900021025     C                   endif
020000050114     C                   endif
020100010601     C*
020200010601     C                   exsr      PREELA
020300010601     C*
020400010604     C* Ebbene...
020500010604     C*
020600010604     C  N31              ADD       1             §CTROK            7 0
020700010604     C   32              ADD       1             §CTRMO            7 0
020800010604     C   31              ADD       1             §CTRNO            7 0
020900020205     C*
021000040301     C* Scarico il buffer del FIVAB
021100021014     C  N31              WRITE     FIVAB000
021200010604     C*
021300020722     C                   endif
021400020722     C*
021500010604     C                   if        *in31 = *off and
021600010604     C                             *in32 = *off
021700010604     C                   eval      vinflg = '1'
021800010604     C                   else
021900010604     C                   eval      recko = vindta
022000010604     C                   except    rigadett
022100010604     C                   eval      vinflg = '2'
022200010604     C                   endif
022300010604     C*
022400010330     C                   else
022500010330     C                   eval      vinflg = '1'
022600010330     C                   endif
022700010601     C*
022800010601     C  N70              update    tivin000
022900010330     C*
023000010330     C  N70              ENDdo
023100010601     C
023200010601     C*
023300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023400010601     C                   if        cntNonEl = *zeros or
023500010601     C                             flgMulti = '0'
023600010330     C* Se non ci sono record con errori ...
023700010601     C                   if        §ctrno = 0 and
023800010604     C                             §ctrmo = 0 and
023900010601     C                             flgStato <> '2'
024000010330     C* ... restituisco esito OK.
024100010330     C                   eval      wrkesito = '0'
024200010330     C                   else
024300010330     C                   if        §ctrok > 0
024400010330     C                   eval      wrkesito = '1'
024500010330     C                   else
024600010615     C                   if        flgOk = '0'
024700010615     C                   eval      wrkesito = '2'
024800010615     C                   else
024900010615     C                   eval      wrkesito = '6'
025000010615     C                   endif
025100010330     C                   endif
025200010330     C                   endif
025300010601     C                   else
025400010601     C                   eval      wrkesito = '9'
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        %open(tivin00r)
025800010330     C                   close     tivin00r
025900010330     C                   endif
026000021014     C                   if        %open(fivabwwr)
026100021014     C                   close     fivabwwr
026200010330     C                   endif
026300010601     C*
026400010601     C                   if        vlrpoi <> 999
026500010601     C                   eval      invfil = vlrpoi
026600010601     C                   endif
026700010330     C*
026800010330     C                   if        §ctrok > 0
026900010601     C                             and invfil > *zeros
027000010330     C                   exsr      invio
027100010330     C                   endif
027200010601     C*
027300010618     C                   if        flgGiro = '1'
027400010601     C                   exsr      endela
027500010618     C                   endif
027600010330     C*
027700010330     C                   ENDSR
027800010330     C***
027900010330
028000010601
028100010601
028200010601
028300010601
028400010330     C*----------------------------------------------------*
028500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028600010330     C*----------------------------------------------------*
028700010330     C     INZVAR        BEGSR
028800010330     C*
028900020204     C                   Z-ADD     *zeros        Num5_0            5 0
029000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029100020322     C                   MOVEL     '0'           FlgCAS            1
029200010330     C*
029300010330     C                   ENDSR
029400010330     C*----------------------------------------------------*
029500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
029600010330     C*----------------------------------------------------*
029700010330     C     DEFCAM        BEGSR
029800010330     C*
029900020204     C* Inizializzo il buffer del record da scrivere
030000021014     C                   CLEAR                   FIVAB000
030100020204     C* Imposto i valori di default...
030200050114     C                   EVAL      VABCCM = 0893012
030300050114     C                   EVAL      VABLNP = 089
030400040713     C                   EVAL      VABCTR = 000
030500050131     C                   EVAL      VABCTM = '7Q'
030600041108     C                   EVAL      VABCBO = '1'
030700050114     C                   EVAL      VABNAS = 'FOTO-OTTICA'
030800020204     C* ... e poi verifico se sono stati passati come parametri
030900020204     C                   IF        vlrppt > *blanks
031000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
031100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031200020204     C                   EXSR      CHKNUM
031300020204     C                   IF        PiInt=*on
031400020204     C                   Z-ADD     PiVal         VABCCM
031500020204     C                   ENDIF
031600040301     C                   ENDIF
031700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
031800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020204     C                   EXSR      CHKNUM
032000020204     C                   IF        PiInt=*on
032100020204     C                   Z-ADD     PiVal         VABLNP
032200020204     C                   ENDIF
032300040301     C                   ENDIF
032400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
032500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032600020204     C                   EXSR      CHKNUM
032700020204     C                   IF        PiInt=*on
032800020204     C                   Z-ADD     PiVal         VABCTR
032900040301     C                   ENDIF
033000020204     C                   ENDIF
033100020204     C                   ENDIF
033200020204     C*
033300010330     C                   ENDSR
033400010607     C*----------------------------------------------------*
033500021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033600010607     C*----------------------------------------------------*
033700010607     C     IMPVAB        BEGSR
033800010607     C*
033900010607     C                   SETOFF                                       3132
034000010607     C*
034100010607     C* Reperimento campi ALFA
034200010607     C*
034300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
034400050114     C                   EVAL      VABRSD=%trim(%subst(vindta:27:35))
034500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034700020117     C* ==
034800050114     C                   EVAL      VABRD2=%trim(%subst(vindta:62:35))
034900050114     C                   EVAL      VABIND=%trim(%subst(vindta:97:35))
035000050114     C                   EVAL      VABLOD=%trim(%subst(vindta:141:35))
035100050114     C                   EVAL      VABPRD=%trim(%subst(vindta:176:2))
035200050201     C                   EVAL      VABRMA=%trim(%subst(vindta:178:15))
035300050114     C                   EVAL      VABTIC=%trim(%subst(vindta:221:2))
035400050114     C                   EVAL      VABNOT=%trim(%subst(vindta:236:35))
035500050201     C                   EVAL      VABNT2=%trim(%subst(vindta:271:35))
035600010607     C*
035700010607     C* Reperimento campi NUMERICI
035800020722     C                   MOVEL     DATCOR        VABAAS
035900020722     C                   MOVE      DATCOR        VABMGS
036000050114     C* CCM
036100050114     C*                  EVAL      PiStr=%trim(%subst(vindta:1:7))
036200050114     C*                  EXSR      CHKNUM
036300050114     C*                  IF        PiInt=*on
036400050114     C*                  Z-ADD     PiVal         VABCCM
036500050114     C*                  ELSE
036600050114     C*                  SETON                                        32
036700050114     C*                  Z-ADD     *zeros        VABCCM
036800050114     C*                  EVAL      vinmsg = %trimr(vinmsg)
036900050114     C*                            + ' ' + 'VABCCM'
037000050114     C*                  ENDIF
037100050114     C* LNP
037200050114     C*                  EVAL      PiStr=%trim(%subst(vindta:8:3))
037300050114     C*                  EXSR      CHKNUM
037400050114     C*                  IF        PiInt=*on
037500050114     C*                  Z-ADD     PiVal         VABLNP
037600050114     C*                  ELSE
037700050114     C*                  SETON                                        32
037800050114     C*                  Z-ADD     *zeros        VABLNP
037900050114     C*                  EVAL      vinmsg = %trimr(vinmsg)
038000050114     C*                            + ' ' + 'VABLNP'
038100050114     C*                  ENDIF
038200050128     C* RMN
038300050201     C                   EVAL      PiStr=%trim(%subst(vindta:193:15))
038400020304     C                   EXSR      CHKNUM
038500020304     C                   IF        PiInt=*on
038600040301     C                   Z-ADD     PiVal         VABRMN
038700020304     C                   ELSE
038800050128     C                   SETON                                        32
038900040301     C                   Z-ADD     *zeros        VABRMN
039000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
039100050128     C                             + ' ' + 'VABRMN'
039200020304     C                   ENDIF
039300050128     C* NSP
039400050128     C                   EVAL      PiStr=%trim(%subst(vindta:193:15))
039500050128     C                   EXSR      CHKNUM
039600050128     C                   IF        PiInt=*on
039700050128     C                   Z-ADD     PiVal         VABNSP
039800050128     C                   ELSE
039900050128     C                   SETON                                        31
040000050128     C                   Z-ADD     *zeros        VABNSP
040100050128     C                   EVAL      vinmsg = %trimr(vinmsg)
040200050128     C                             + ' ' + 'VABNSP'
040300050128     C                   ENDIF
040400040420     C* CAD
040500050114     C                   EVAL      PiStr=%trim(%subst(vindta:132:9))
040600010607     C                   EXSR      CHKNUM
040700010607     C                   IF        PiInt=*on
040800020204     C                   Z-ADD     PiVal         Num5_0
040900020204     C                   MOVEL(p)  Num5_0        VABCAD
041000010607     C                   ELSE
041100010607     C                   SETON                                        32
041200020204     C                   EVAL      VABCAD = *zeros
041300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041400020204     C                             + ' ' + 'VABCAD'
041500010607     C                   ENDIF
041600040420     C* NCL
041700050114     C                   EVAL      PiStr=%trim(%subst(vindta:306:5))
041800010607     C                   EXSR      CHKNUM
041900010607     C                   IF        PiInt=*on
042000010607     C                   Z-ADD     PiVal         VABNCL
042100010607     C                   ELSE
042200010607     C                   SETON                                        32
042300010607     C                   Z-ADD     *zeros        VABNCL
042400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042500010607     C                             + ' ' + 'VABNCL'
042600010607     C                   ENDIF
042700040420     C* PKB
042800050114     C                   EVAL      PiStr=%trim(%subst(vindta:311:7))
042900010607     C                   EXSR      CHKNUM
043000010607     C                   IF        PiNum=*on
043100050114     C                   Z-ADD     PiVal         VABPKB
043200010607     C                   ELSE
043300010607     C                   SETON                                        32
043400010607     C                   Z-ADD     *zeros        VABPKB
043500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043600010607     C                             + ' ' + 'VABPKB'
043700010607     C                   ENDIF
043800041103     C* CAS
043900050114     C                   EVAL      PiStr=%trim(%subst(vindta:208:13))
044000041103     C                   EXSR      CHKNUM
044100041103     C                   IF        PiNum=*on
044200050114     C                   Z-ADD     PiVal         VABCAS
044300050114     C                   IF        VABCAS > *zeros
044400041103     C                   MOVEL     '1'           FlgCAS
044500041108     C                   EVAL      VABVCA = 'EUR'
044600041103     C                   ENDIF
044700041103     C                   ELSE
044800041103     C                   SETON                                        32
044900041103     C                   Z-ADD     *zeros        VABCAS
045000041103     C                   EVAL      vinmsg = %trimr(vinmsg)
045100041103     C                             + ' ' + 'VABCAS'
045200041103     C                   ENDIF
045300041203     C* IAS
045400050114     C                   EVAL      PiStr=%trim(%subst(vindta:223:13))
045500041203     C                   EXSR      CHKNUM
045600041203     C                   IF        PiNum=*on
045700050114     C                   Z-ADD     PiVal         VABIAS
045800050114     C                   IF        VABIAS > *zeros
045900041203     C                   EVAL      VABVAS = 'EUR'
046000041203     C                   ENDIF
046100041203     C                   ELSE
046200041203     C                   SETON                                        32
046300041203     C                   Z-ADD     *zeros        VABIAS
046400041203     C                   EVAL      vinmsg = %trimr(vinmsg)
046500041203     C                             + ' ' + 'VABIAS'
046600041203     C                   ENDIF
046700020322     C*
046800020322     C* Considerazioni finali su CBO/CAS
046900020322     C                   IF        FlgCAS = '1'
047000040713     C                   IF        VABCBO = '1'
047100020322     C                   EVAL      VABCBO = '4'
047200040713     C                   ENDIF
047300040713     C                   IF        VABCBO = '2'
047400040713     C                   EVAL      VABCBO = '6'
047500040713     C                   ENDIF
047600020322     C                   ENDIF
047700020204     C*
047800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047900020204     C                   EXSR      CHKIMPDIV
048000020204     C*
048100010607     C                   ENDSR
048200010607     C*----------------------------------------------------*
048300010601
048400020204
048500020204     C*----------------------------------------------------*
048600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048700020204     C*----------------------------------------------------*
048800020204     C     CHKIMPDIV     BEGSR
048900020204     C*
049000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049100020204     C                   Z-ADD     *zeros        wrkDec            9 9
049200020204     C*
049300020204     C* Come prima cosa effettuo considerazioni sulla divisa
049400020204     C                   IF        vabIAS > *zeros
049500020204     C                   IF        vabVAS <> 'EUR'
049600020204     C                   EVAL      vabVAS =  'ITL'
049700020204     C                   ENDIF
049800020204     C                   ENDIF
049900020204     C*
050000020204     C                   IF        vabCAS > *zeros
050100020204     C                   IF        vabVCA <> 'EUR'
050200020204     C                   EVAL      vabVCA =  'ITL'
050300020204     C                   ENDIF
050400020204     C                   ENDIF
050500020204     C*
050600020204     C                   IF        vabVMD > *zeros
050700020321     C                   IF        vabVAD <> 'EUR'
050800020204     C                   EVAL      vabVAD =  'ITL'
050900020204     C                   ENDIF
051000020204     C                   ENDIF
051100020204     C*
051200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051300020204     C                   Z-ADD     vabIAS        wrkDec
051400020204     C                   IF        wrkDec > *zeros
051500020204     C                   IF        vabVAS = 'ITL'
051600020204     C                   EVAL      vabIAS = *zeros
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052100020204     C                   Z-ADD     vabCAS        wrkDec
052200020204     C                   IF        wrkDec > *zeros
052300020204     C                   IF        vabVCA = 'ITL'
052400020204     C                   EVAL      vabCAS = *zeros
052500020204     C                   ENDIF
052600020204     C                   ENDIF
052700020204     C*
052800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052900020204     C                   Z-ADD     vabVMD        wrkDec
053000020204     C                   IF        wrkDec > *zeros
053100020204     C                   IF        vabVAD = 'ITL'
053200020204     C                   EVAL      vabVMD = *zeros
053300020204     C                   ENDIF
053400020204     C                   ENDIF
053500020204     C*
053600020204     C                   ENDSR
053700020204     C***
053800020204
053900010330
054000010330
054100010330     C*----------------------------------------------------*
054200010330     C*  CONTROLLO NUMERICITA' CAMPI
054300010330     C*----------------------------------------------------*
054400010330     C     CHKNUM        BEGSR
054500010330     C*
054600010606     C                   IF        PiDecChr = *blanks
054700050208     C                   EVAL      PiDecChr = '.'
054800010606     C                   ENDIF
054900010606     C*
055000010606     C                   CALL(e)   'ISNUMERIC'
055100010330     C                   PARM                    PiStr            30
055200010606     C                   PARM                    PiDecChr          1
055300010330     C                   PARM      *ZEROS        PiVal            30 9
055400010330     C                   PARM      '0'           PiInt             1
055500010330     C                   PARM      '0'           PiNum             1
055600010330     C                   IF        %error
055700010606     C                   EVAL      PiNum=*off
055800010330     C                   ENDIF
055900010330     C*
056000010330     C                   ENDSR
056100010330     C***
056200010330
056300010601
056400010601
056500010601
056600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
056700010601     C     repfil        BEGSR
056800010601     C*
056900010601     C                   if        invfil = *zeros and
057000010601     C                             depfil > *zeros and
057100010629     C                             (vinflg = *blanks or
057200010629     C                              vinflg = *zeros)
057300010601     C
057400010601     C                   eval      invfil = depfil
057500010601     C                   endif
057600010601     C*
057700010601     C                   if        depfil <> invfil and
057800010601     C                             invfil > *zeros
057900010601     C                   eval      flgMulti = '1'
058000010601     C                   if        vinflg = *blanks
058100010601     C                   add       1             cntNonEl
058200010601     C                   endif
058300010601     C                   endif
058400010601     C*
058500010601     C                   if        vinflg = '2'
058600010601     C                   eval      flgStato = '2'
058700010601     C                   endif
058800010601     C*
058900010601     C                   ENDSR
059000010601     C***
059100010601
059200010601
059300010601
059400010330
059500010330
059600010330
059700990920      /TITLE Invio dei dati al punto operativo.
059800000613     C     invio         BEGSR
059900990920     C*
060000990920     C                   reset                   dscmz
060100010601     C                   move      invfil        cmzdst
060200990920     C                   eval      cmzfld = vlrfou
060300990920     C                   eval      cmzmbd = vlrhdl
060400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060500000710     C                   if        prmfir = *blanks
060600021014     C                   eval      cmzfla = 'FIVAB00F'
060700021014     C                   eval      cmzmba = 'FIVAB00F'
060800000710     C                   else
060900000710     C                   eval      cmzfla = prmfir
061000000710     C                   eval      cmzmba = prmfir
061100000710     C                   endif
061200990920     C                   eval      cmznrr = *zeros
061300990920     C                   move      §ctrok        cmznrr
061400021018     C                   eval      cmzlba = vlrfl1
061500990920     C                   call(e)   'TIS711C'
061600990920     C                   parm                    dscmz
061700990921     C                   parm      *blanks       esito
061800990920     C                   if        %error
061900990920     C                             or cmzerr = '1'
062000990921     C                             or esito  = '1'
062100000710     C                   eval      wrkesito = '3'
062200990920     C                   endif
062300990920     C*
062400000613     C                   ENDSR
062500990910
062600010601
062700010601
062800010601
062900010601
063000010601      /TITLE Invio dei dati al punto operativo.
063100010601     C     opeini        BEGSR
063200010601     C*
063300010601     C* Inizializzo flag e contatori operativi
063400010601     C                   movel     '0'           flgGiro           1
063500010601     C                   movel     '0'           flgMulti          1
063600010601     C                   movel     '1'           flgStato          1
063700010615     C                   movel     '0'           flgOk             1
063800010601     C                   z-add     *zeros        cntNonEl         10 0
063900010601     C                   z-add     *zeros        depfil            3 0
064000010601     C                   z-add     *zeros        invfil            3 0
064100010601     C*
064200010601     C                   ENDSR
064300010601     C***
064400010601
064500010601
064600010601
064700010330
064800010330
064900000613     C     *inzsr        BEGSR
065000990910     C*
065100990910     C     *entry        plist
065200990920     C                   parm                    tivlrds
065300990921     C                   parm      wrkesito      esito
065400000724     C                   parm                    prmlit
065500000710     C                   parm                    prmfir
065600010330     C*
065700010330     C* CALCOLA LA DATA CORRENTE
065800010330     C                   time                    wn14             14 0
065900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066000010330     C                   z-add     wn8           g08dat
066100010330     C                   z-add     *zeros        g08inv
066200010330     C                   movel     '0'           g08err
066300010330     C                   call      'XSRDA8'
066400010330     C                   parm                    wlbda8
066500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
066600000613     C*
066700000613     C                   ENDSR
066800000613     C***
066900990908
067000050114     Otitvg7p   E            riepilogo         2
067100990915     O                                              'Upload via Internet'
067200990915     O                                           +1 'Traduzione TIVIN00R -'
067300021014     O                                           +1 'FIVABWWR'
067400990915     O                                           +1 'Testate bolle'
067500990915     O          E            riepilogo   2
067600990915     O                       wrkdata
067700990915     O                       wrkora              +1
067800990915     O                       procname            +1
067900990915     O          E            riepilogo   2
068000990915     O                                              'Cliente..................:'
068100010605     O                       VABCCM        z     +1
068200990915     O          E            riepilogo   2
068300990920     O                                              'Riferimento Strategi.....:'
068400990920     O                       vlrhdl              +1
068500990915     O          E            riepilogo   2
068600990915     O                                              'Giusti...................:'
068700971022     O                       §CTROK        2   +  1
068800990915     O          E            riepilogo   2
068900990915     O                                              'Sbagliati e corretti.....:'
069000971022     O                       §CTRMO        2   +  1
069100990915     O          E            riepilogo   2
069200990915     O                                              'Sbagliati e scartati.....:'
069300971022     O                       §CTRNO        2   +  1
069400000613
069500050114     Otitvg7ps  E            testdett          2
069600000613     O                                              'Upload via Internet'
069700000613     O                                           +1 'Traduzione TIVIN00R -'
069800021014     O                                           +1 'FIVABWWR'
069900000613     O                                           +1 'Testate bolle'
070000000616     O          E            testdett    3
070100000613     O                                           +2 'N° rec'
070200000613     O                                           +3 'Anteprima contenuto'
070300000616     O          E            rigadett    2
070400000613     O                       rrnum               +2
070500000621     O                       recko               +3
070600000616     O          E            findett     2
070700000613     O                       wrkdata
070800000613     O                       wrkora              +1
070900000613     O                       procname            +1
071000000616     O          E            findett     2
071100000613     O                                              'Cliente..................:'
071200010605     O                       VABCCM        z     +1
071300000616     O          E            findett     2
071400000613     O                                              'Riferimento Strategi.....:'
071500000613     O                       vlrhdl              +1
071600000616     O          E            findett     2
071700000613     O                                              'Giusti...................:'
071800000613     O                       §CTROK        2   +  1
071900000616     O          E            findett     2
072000000613     O                                              'Sbagliati e corretti.....:'
072100000613     O                       §CTRMO        2   +  1
072200000616     O          E            findett     2
072300000613     O                                              'Sbagliati e scartati.....:'
072400000613     O                       §CTRNO        2   +  1
072500000613** CMD - COMANDI CL
072600050114OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
072700050114OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVG7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
072800050114DLTOVR FILE(TITVG7P TITVG7PS) LVL(*)
072900010606
073000000613
