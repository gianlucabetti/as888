000100050527      /TITLE Upload via Internet: traduzione in EDIVABWR. (x SAFILO Cod. 0061602)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050330     FEDIVABWR  O    E             DISK    usropn
000700050330     Ftitvl7p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900050330     Ftitvl7ps  O    f  198        PRINTER usropn
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
002300050330     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300050330     D wNomeFile       s             30    INZ(*blanks)
003400050330     D wCMR            s             35    INZ(*blanks)
003500050330     D wCCM            s              7  0 INZ(*zeros)
003600050330     D wPosDaA         s              2    INZ(*blanks)
003700050330     D wPosDa          s              2  0 INZ(*zeros)
003800050330     D wLungA          s              2    INZ(*blanks)
003900050330     D wLung           s              2  0 INZ(*zeros)
004000010330     D*------------------
004100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200010330     D*------------------
004300010330     D WLBDA8          DS                  INZ
004400010330     D  G08DAT                 1      8  0
004500010330     D  G08INV                 9     16  0
004600010330     D  G08ERR                17     17
004700010330     D  G08TGI                18     22  0
004800050330     D*------------------
004900050330     D* DS REPERIMENTO NUMERATORE
005000050330     D*------------------
005100050330     D trul33ds      e ds                  inz
005200050330     D*------------------
005300050330     D* DS ARCHITETTURA
005400050330     D*------------------
005500050330     D kpjba         e ds                  inz
005600050330     D*------------------
005700050330     D* Costanti
005800050330     D*------------------
005900050330     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006000050330     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006100000313     D*
006200010330
006300010330
006400990915     C                   time                    wrkdata
006500990915     C                   time                    wrkora
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900010601     C*
007000010601     C                   exsr      opeini
007100010605     C                   exsr      rwvab
007200010601     C*
007300010601     C                   seton                                        lr
007400010601
007500010601
007600010601
007700010601
007800010601     C*--------------------------------------------------------
007900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008000010601     C*--------------------------------------------------------
008100010601     C     PREELA        BEGSR
008200010601     C*
008300010601     C* SE OCCORRE SPEDIRE IN FILIALE
008400010601     C                   if        invfil <> *zeros and
008500010601     C                             flgGiro = '0'
008600010601     C*
008700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008800010601     C                   eval      flgGiro = '1'
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200010601     C     invfil        chain     azorg01l
009300000724     C                   if        %found
009400000724     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000724     C*
009700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000724     C                   PARM                    depcmd
010100000724     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000616     C*
010500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010600000616     C                   movel(p)  CMD(2)        depcmd
010700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010800000616     C*
010900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011000011113     C                   Z-ADD     150           LENGH            15 5
011100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000616     C                   PARM                    depcmd
011300000616     C                   PARM                    LENGH
011400000613     C*
011500050330     C                   if        not %open(titvl7ps)
011600050330     C                   open      titvl7ps
011700000616     C                   except    testdett
011800000613     C                   endif
011900010601     C*
012000010601     C                   endif
012100010601     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100990915     C*
013200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013300000613     C*
013400050330     C                   if        %open(titvl7ps)
013500000616     C                   except    findett
013600050330     C                   close     titvl7ps
013700000613     C                   endif
013800000616     C*
013900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014000011113     C                   Z-ADD     150           LENGH            15 5
014100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014200000616     C                   PARM                    CMD(3)
014300000616     C                   PARM                    LENGH
014400000616     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900000613
015000000613     C*--------------------------------------------------------
015100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015200000613     C*--------------------------------------------------------
015300000621     C     STPR          BEGSR
015400000613     C*
015500050330     C                   if        not %open(titvl7p)
015600050330     C                   open      titvl7p
015700990915     C                   endif
015800990915     C*
015900990915     C                   except    riepilogo
016000990915     C*
016100050330     C                   if        %open(titvl7p)
016200050330     C                   close     titvl7p
016300990914     C                   endif
016400000613     C*
016500000613     C                   ENDSR
016600000613     C***
016700000613
016800010601
016900010601
017000010330     C*--------------------------------------------------------
017100050330     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
017200010330     C*--------------------------------------------------------
017300010605     C     RWVAB         BEGSR
017400010330     C*
017500050330     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
017600050330     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
017700050330     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
017800050330     C                   if        *in60 = *on
017900010330     C                   if        not %open(tivin00r)
018000010330     C                   open      tivin00r
018100010330     C                   endif
018200050330     C                   if        not %open(edivabwr)
018300050330     C                   open      edivabwr
018400010330     C                   endif
018500010330     C*
018600010604     C                   clear                   §CTROK
018700010604     C                   clear                   §CTRMO
018800010604     C                   clear                   §CTRNO
018900010330     C*
019000010330     C                   DO        *HIVAL
019100010330     C*
019200010330     C                   READ      tivin00r                               70
019300010618     C*
019400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019500010618     C                   if        vinflg = '1'
019600010618     C                   eval      flgOk = '1'
019700010618     C                   endif
019800010618     C*
019900010330     C                   if        vindta > *blanks
020000010330     C                   add       1             rrnum
020100010330     C*
020200010601     C                   if        *in70 = *off and
020300010330     C                             (vinflg = *blanks
020400010330     C                              or vinflg = '0'
020500010330     C                              or vinflg = '2')
020600010330     C*
020700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020800010711     C                   if        vinflg = *blanks or vinflg = '0'
020900010711     C                   clear                   vinmsg
021000010711     C                   endif
021100010601     C*
021200010330     C                   exsr      inzvar
021300010330     C                   exsr      defcam
021400010605     C                   exsr      impvab
021500050330     C*
021600050330     C* Effettuo alcune forzature
021700050330     C                   if        wCCM <> *zeros
021800050330     C                   eval      VABCCM = wCCM
021900050330     C                   endif
022000050330     C*
022100050330     C* Valorizzo i campi relativi al "CMR"
022200050330     C                   eval      VABCMR = wCMR
022300050330     C                   eval      VABDCM = *zeros
022400050330     C                   eval      VABDTS = *zeros
022500050330     C                   eval      VABHMS = *zeros
022600050330     C                   eval      VABCNT = 1
022700010601     C*
022800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022900010605     C                   eval      depfil = VABLNP
023000010601     C                   exsr      repfil
023100010601     C                   if        depfil = invfil
023200021025     C                   if        vlrpoi = 999
023300021025     C                   MOVE(P)   invfil        VABFGS
023400021025     C                   else
023500021025     C                   MOVE(P)   vlrpoi        VABFGS
023600021025     C                   endif
023700010601     C*
023800010601     C                   exsr      PREELA
023900010601     C*
024000010604     C* Ebbene...
024100010604     C*
024200010604     C  N31              ADD       1             §CTROK            7 0
024300010604     C   32              ADD       1             §CTRMO            7 0
024400010604     C   31              ADD       1             §CTRNO            7 0
024500020205     C*
024600050330     C* Scarico il buffer del EDIVAB
024700050330     C  N31              WRITE     EDIVAB00
024800010604     C*
024900020722     C                   endif
025000020722     C*
025100010604     C                   if        *in31 = *off and
025200010604     C                             *in32 = *off
025300010604     C                   eval      vinflg = '1'
025400010604     C                   else
025500010604     C                   eval      recko = vindta
025600010604     C                   except    rigadett
025700010604     C                   eval      vinflg = '2'
025800010604     C                   endif
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700010330     C  N70              ENDdo
026800050330     C*
026900050330     C                   endif
027000010601     C*
027100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027200010601     C                   if        cntNonEl = *zeros or
027300010601     C                             flgMulti = '0'
027400010330     C* Se non ci sono record con errori ...
027500010601     C                   if        §ctrno = 0 and
027600010604     C                             §ctrmo = 0 and
027700010601     C                             flgStato <> '2'
027800010330     C* ... restituisco esito OK.
027900010330     C                   eval      wrkesito = '0'
028000010330     C                   else
028100010330     C                   if        §ctrok > 0
028200010330     C                   eval      wrkesito = '1'
028300010330     C                   else
028400010615     C                   if        flgOk = '0'
028500010615     C                   eval      wrkesito = '2'
028600010615     C                   else
028700010615     C                   eval      wrkesito = '6'
028800010615     C                   endif
028900010330     C                   endif
029000010330     C                   endif
029100010601     C                   else
029200010601     C                   eval      wrkesito = '9'
029300010601     C                   endif
029400010330     C*
029500010330     C                   if        %open(tivin00r)
029600010330     C                   close     tivin00r
029700010330     C                   endif
029800050330     C                   if        %open(edivabwr)
029900050330     C                   close     edivabwr
030000010330     C                   endif
030100010601     C*
030200010601     C                   if        vlrpoi <> 999
030300010601     C                   eval      invfil = vlrpoi
030400010601     C                   endif
030500010330     C*
030600010330     C                   if        §ctrok > 0
030700010601     C                             and invfil > *zeros
030800010330     C                   exsr      invio
030900010330     C                   endif
031000010601     C*
031100010618     C                   if        flgGiro = '1'
031200010601     C                   exsr      endela
031300010618     C                   endif
031400010330     C*
031500010330     C                   ENDSR
031600010330     C***
031700010330
031800010601
031900010601
032000010601
032100010601
032200010330     C*----------------------------------------------------*
032300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032400010330     C*----------------------------------------------------*
032500010330     C     INZVAR        BEGSR
032600010330     C*
032700020204     C                   Z-ADD     *zeros        Num5_0            5 0
032800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032900020322     C                   MOVEL     '0'           FlgCAS            1
033000010330     C*
033100010330     C                   ENDSR
033200010330     C*----------------------------------------------------*
033300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
033400010330     C*----------------------------------------------------*
033500010330     C     DEFCAM        BEGSR
033600010330     C*
033700020204     C* Inizializzo il buffer del record da scrivere
033800050330     C                   CLEAR                   EDIVAB00
033900020204     C* Imposto i valori di default...
034000050628     C                   EVAL      VABCCM = 0061602
034100050628     C                   EVAL      VABLNP = 006
034200050628     C                   EVAL      VABCTR = 300
034300041108     C                   EVAL      VABCBO = '1'
034400020204     C* ... e poi verifico se sono stati passati come parametri
034500050628     C*                  IF        vlrppt > *blanks
034600050628     C*                  IF        %trim(%subst(vlrppt:1:7)) <> *blanks
034700050628     C*                  EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034800050628     C*                  EXSR      CHKNUM
034900050628     C*                  IF        PiInt=*on
035000050628     C*                  Z-ADD     PiVal         VABCCM
035100050628     C*                  ENDIF
035200050628     C*                  ENDIF
035300050628     C*                  IF        %trim(%subst(vlrppt:8:3)) <> *blanks
035400050628     C*                  EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035500050628     C*                  EXSR      CHKNUM
035600050628     C*                  IF        PiInt=*on
035700050628     C*                  Z-ADD     PiVal         VABLNP
035800050628     C*                  ENDIF
035900050628     C*                  ENDIF
036000050628     C*                  IF        %trim(%subst(vlrppt:11:3)) <> *blanks
036100050628     C*                  EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036200050628     C*                  EXSR      CHKNUM
036300050628     C*                  IF        PiInt=*on
036400050628     C*                  Z-ADD     PiVal         VABCTR
036500050628     C*                  ENDIF
036600050628     C*                  ENDIF
036700050628     C*                  ENDIF
036800020204     C*
036900010330     C                   ENDSR
037000010607     C*----------------------------------------------------*
037100050330     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
037200010607     C*----------------------------------------------------*
037300010607     C     IMPVAB        BEGSR
037400010607     C*
037500010607     C                   SETOFF                                       3132
037600010607     C*
037700010607     C* Reperimento campi ALFA
037800010607     C*
037900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
038000050330     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
038100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
038300020117     C* ==
038400050330     C                   EVAL      VABRD2=%trim(%subst(vindta:92:15))
038500050330     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
038600050330     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
038700050330     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
038800050330     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
038900050330     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
039000050330     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
039100050330     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
039200050330     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
039300050330     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:35))
039400050330     C                   IF        %subst(vindta:16:1)<>'0'
039500050330     C                   EVAL      VABFFD='S'
039600050322     C                   ENDIF
039700050330     C                   IF        %subst(vindta:390:1)<>*blanks
039800050330     C                   EVAL      VABCBO='2'
039900050322     C                   ENDIF
040000010607     C*
040100010607     C* Reperimento campi NUMERICI
040200020722     C                   MOVEL     DATCOR        VABAAS
040300020722     C                   MOVE      DATCOR        VABMGS
040400050330     C* NSP/RMN
040500050330     C*                  EVAL      PiStr=%trim(%subst(vindta:187+1:10-1))
040600050330     C*                  EXSR      CHKNUM
040700050330     C*                  IF        PiInt=*on
040800050330     C*                  Z-ADD     PiVal         VABNSP
040900050330     C*                  Z-ADD     PiVal         VABRMN
041000050330     C*                  ELSE
041100050330     C*                  SETON                                        31
041200050330     C*                  Z-ADD     *zeros        VABNSP
041300050330     C*                  Z-ADD     *zeros        VABRMN
041400050330     C*                  EVAL      vinmsg = %trimr(vinmsg)
041500050330     C*                            + ' ' + 'VABNSP VABRMN'
041600050330     C*                  ENDIF
041700050330     C* NSP/RMN => Stacco un numeratore da AZNUM
041800050330     C                   clear                   TRUL33DS
041900050330     C                   eval      I33OPE = *zeros
042000050330     C                   eval      I33CNU = 302
042100050330     C                   eval      I33NUM = 1
042200050330     C                   movel     TRUL33DS      KPJBU
042300050330     C                   call      'TRUL33R'
042400050330     C                   parm                    KPJBA
042500050330     C                   movel     KPJBU         TRUL33DS
042600050330     C                   if        O33ERR = *zeros
042700050330     C                   z-add     O33NRF        VABNSP
042800050330     C                   z-add     O33NRF        VABRMN
042900050330     C                   else
043000050330     C                   SETON                                        31
043100050330     C                   EVAL      vinmsg = %trimr(vinmsg)
043200050330     C                             + ' ' + 'VABNSP VABRMN'
043300050330     C                   endif
043400040420     C* CAD
043500050330     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
043600010607     C                   EXSR      CHKNUM
043700010607     C                   IF        PiInt=*on
043800020204     C                   Z-ADD     PiVal         Num5_0
043900020204     C                   MOVEL(p)  Num5_0        VABCAD
044000010607     C                   ELSE
044100010607     C                   SETON                                        32
044200020204     C                   EVAL      VABCAD = *zeros
044300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044400020204     C                             + ' ' + 'VABCAD'
044500010607     C                   ENDIF
044600040420     C* NCL
044700050330     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
044800010607     C                   EXSR      CHKNUM
044900010607     C                   IF        PiInt=*on
045000010607     C                   Z-ADD     PiVal         VABNCL
045100010607     C                   ELSE
045200010607     C                   SETON                                        32
045300010607     C                   Z-ADD     *zeros        VABNCL
045400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045500010607     C                             + ' ' + 'VABNCL'
045600010607     C                   ENDIF
045700040420     C* PKB
045800050330     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
045900010607     C                   EXSR      CHKNUM
046000010607     C                   IF        PiNum=*on
046100050330     C                   Z-ADD(H)  PiVal         VABPKB
046200010607     C                   ELSE
046300010607     C                   SETON                                        32
046400010607     C                   Z-ADD     *zeros        VABPKB
046500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
046600010607     C                             + ' ' + 'VABPKB'
046700010607     C                   ENDIF
046800050330     C* VLB
046900050330     C                   IF        %subst(vindta:308:7)<>*blanks
047000050330     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
047100050330     C                   EXSR      CHKNUM
047200050330     C                   IF        PiNum=*on
047300050330     C                   Z-ADD(H)  PiVal         VABVLB
047400050330     C                   ELSE
047500050330     C                   SETON                                        32
047600050330     C                   Z-ADD     *zeros        VABVLB
047700050330     C                   EVAL      vinmsg = %trimr(vinmsg)
047800050330     C                             + ' ' + 'VABVLB'
047900050330     C                   ENDIF
048000050330     C                   ENDIF
048100050330     C* CAS
048200050330     C                   IF        %subst(vindta:280:12) <> '000000000,00'
048300050330     C                   EVAL      FlgCAS = '1'
048400050330     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
048500041103     C                   EXSR      CHKNUM
048600041103     C                   IF        PiNum=*on
048700041103     C                   Z-ADD     PiVal         VABCAS
048800041103     C                   ELSE
048900041103     C                   SETON                                        32
049000041103     C                   Z-ADD     *zeros        VABCAS
049100041103     C                   EVAL      vinmsg = %trimr(vinmsg)
049200041103     C                             + ' ' + 'VABCAS'
049300041103     C                   ENDIF
049400050330     C                   ENDIF
049500050330     C* IAS
049600050330     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049700050330     C                   EXSR      CHKNUM
049800050330     C                   IF        PiNum=*on
049900050330     C                   Z-ADD     PiVal         VABIAS
050000050330     C                   ELSE
050100050330     C                   SETON                                        32
050200050330     C                   Z-ADD     *zeros        VABIAS
050300050330     C                   EVAL      vinmsg = %trimr(vinmsg)
050400050330     C                             + ' ' + 'VABIAS'
050500050330     C                   ENDIF
050600020322     C*
050700020322     C* Considerazioni finali su CBO/CAS
050800020322     C                   IF        FlgCAS = '1'
050900040713     C                   IF        VABCBO = '1'
051000020322     C                   EVAL      VABCBO = '4'
051100040713     C                   ENDIF
051200040713     C                   IF        VABCBO = '2'
051300040713     C                   EVAL      VABCBO = '6'
051400040713     C                   ENDIF
051500020322     C                   ENDIF
051600020204     C*
051700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051800020204     C                   EXSR      CHKIMPDIV
051900020204     C*
052000010607     C                   ENDSR
052100010607     C*----------------------------------------------------*
052200010601
052300020204
052400020204     C*----------------------------------------------------*
052500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052600020204     C*----------------------------------------------------*
052700020204     C     CHKIMPDIV     BEGSR
052800020204     C*
052900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053000020204     C                   Z-ADD     *zeros        wrkDec            9 9
053100020204     C*
053200020204     C* Come prima cosa effettuo considerazioni sulla divisa
053300020204     C                   IF        vabIAS > *zeros
053400020204     C                   IF        vabVAS <> 'EUR'
053500020204     C                   EVAL      vabVAS =  'ITL'
053600020204     C                   ENDIF
053700020204     C                   ENDIF
053800020204     C*
053900020204     C                   IF        vabCAS > *zeros
054000020204     C                   IF        vabVCA <> 'EUR'
054100020204     C                   EVAL      vabVCA =  'ITL'
054200020204     C                   ENDIF
054300020204     C                   ENDIF
054400020204     C*
054500020204     C                   IF        vabVMD > *zeros
054600020321     C                   IF        vabVAD <> 'EUR'
054700020204     C                   EVAL      vabVAD =  'ITL'
054800020204     C                   ENDIF
054900020204     C                   ENDIF
055000020204     C*
055100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055200020204     C                   Z-ADD     vabIAS        wrkDec
055300020204     C                   IF        wrkDec > *zeros
055400020204     C                   IF        vabVAS = 'ITL'
055500020204     C                   EVAL      vabIAS = *zeros
055600020204     C                   ENDIF
055700020204     C                   ENDIF
055800020204     C*
055900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
056000020204     C                   Z-ADD     vabCAS        wrkDec
056100020204     C                   IF        wrkDec > *zeros
056200020204     C                   IF        vabVCA = 'ITL'
056300020204     C                   EVAL      vabCAS = *zeros
056400020204     C                   ENDIF
056500020204     C                   ENDIF
056600020204     C*
056700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056800020204     C                   Z-ADD     vabVMD        wrkDec
056900020204     C                   IF        wrkDec > *zeros
057000020204     C                   IF        vabVAD = 'ITL'
057100020204     C                   EVAL      vabVMD = *zeros
057200020204     C                   ENDIF
057300020204     C                   ENDIF
057400020204     C*
057500020204     C                   ENDSR
057600020204     C***
057700020204
057800010330
057900010330
058000010330     C*----------------------------------------------------*
058100010330     C*  CONTROLLO NUMERICITA' CAMPI
058200010330     C*----------------------------------------------------*
058300010330     C     CHKNUM        BEGSR
058400010330     C*
058500010606     C                   IF        PiDecChr = *blanks
058600010606     C                   EVAL      PiDecChr = ','
058700010606     C                   ENDIF
058800010606     C*
058900010606     C                   CALL(e)   'ISNUMERIC'
059000010330     C                   PARM                    PiStr            30
059100010606     C                   PARM                    PiDecChr          1
059200010330     C                   PARM      *ZEROS        PiVal            30 9
059300010330     C                   PARM      '0'           PiInt             1
059400010330     C                   PARM      '0'           PiNum             1
059500010330     C                   IF        %error
059600010606     C                   EVAL      PiNum=*off
059700010330     C                   ENDIF
059800010330     C*
059900010330     C                   ENDSR
060000010330     C***
060100010330
060200010601
060300010601
060400010601
060500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
060600010601     C     repfil        BEGSR
060700010601     C*
060800010601     C                   if        invfil = *zeros and
060900010601     C                             depfil > *zeros and
061000010629     C                             (vinflg = *blanks or
061100010629     C                              vinflg = *zeros)
061200010601     C
061300010601     C                   eval      invfil = depfil
061400010601     C                   endif
061500010601     C*
061600010601     C                   if        depfil <> invfil and
061700010601     C                             invfil > *zeros
061800010601     C                   eval      flgMulti = '1'
061900010601     C                   if        vinflg = *blanks
062000010601     C                   add       1             cntNonEl
062100010601     C                   endif
062200010601     C                   endif
062300010601     C*
062400010601     C                   if        vinflg = '2'
062500010601     C                   eval      flgStato = '2'
062600010601     C                   endif
062700010601     C*
062800010601     C                   ENDSR
062900010601     C***
063000010601
063100010601
063200010601
063300010330
063400010330
063500010330
063600990920      /TITLE Invio dei dati al punto operativo.
063700000613     C     invio         BEGSR
063800990920     C*
063900990920     C                   reset                   dscmz
064000010601     C                   move      invfil        cmzdst
064100990920     C                   eval      cmzfld = vlrfou
064200990920     C                   eval      cmzmbd = vlrhdl
064300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
064400000710     C                   if        prmfir = *blanks
064500050330     C                   eval      cmzfla = 'EDIVAB0F'
064600050330     C                   eval      cmzmba = 'EDIVAB0F'
064700000710     C                   else
064800000710     C                   eval      cmzfla = prmfir
064900000710     C                   eval      cmzmba = prmfir
065000000710     C                   endif
065100990920     C                   eval      cmznrr = *zeros
065200990920     C                   move      §ctrok        cmznrr
065300021018     C                   eval      cmzlba = vlrfl1
065400990920     C                   call(e)   'TIS711C'
065500990920     C                   parm                    dscmz
065600990921     C                   parm      *blanks       esito
065700990920     C                   if        %error
065800990920     C                             or cmzerr = '1'
065900990921     C                             or esito  = '1'
066000000710     C                   eval      wrkesito = '3'
066100990920     C                   endif
066200990920     C*
066300000613     C                   ENDSR
066400990910
066500010601
066600010601
066700010601
066800010601
066900010601      /TITLE Invio dei dati al punto operativo.
067000010601     C     opeini        BEGSR
067100010601     C*
067200010601     C* Inizializzo flag e contatori operativi
067300010601     C                   movel     '0'           flgGiro           1
067400010601     C                   movel     '0'           flgMulti          1
067500010601     C                   movel     '1'           flgStato          1
067600010615     C                   movel     '0'           flgOk             1
067700010601     C                   z-add     *zeros        cntNonEl         10 0
067800010601     C                   z-add     *zeros        depfil            3 0
067900010601     C                   z-add     *zeros        invfil            3 0
068000010601     C*
068100010601     C                   ENDSR
068200010601     C***
068300010601
068400010601
068500010601
068600010330
068700010330
068800000613     C     *inzsr        BEGSR
068900990910     C*
069000990910     C     *entry        plist
069100990920     C                   parm                    tivlrds
069200990921     C                   parm      wrkesito      esito
069300000724     C                   parm                    prmlit
069400000710     C                   parm                    prmfir
069500050330     C*
069600050330     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
069700050330     C                   move(p)   vlrMSG        wNomeFile
069800050330     C*
069900050330     C* Porto in maiuscolo il nome del file così reperito
070000050330     C     minu:maiu     XLATE     wNomeFile     wNomeFile
070100050330     C*
070200050330     C* In base al prefisso del nome file riconosco a quale tipo traffico i dai si riferiscono
070300050330     C* ed abbino il codice cliente relativo:
070400050330     C* - traffico MERCE       => codice cliente 1020001
070500050330     C* - traffico PUBBLICITA' => codice cliente 1020002
070600050330     C* - traffico RICAMBI     => codice cliente 1020003
070700050330     C*
070800050330     C                   if        %subst(wNomeFile:1:3) = 'TBA' or
070900050330     C                             %subst(wNomeFile:1:3) = 'TBM' or
071000050330     C                             %subst(wNomeFile:1:3) = 'TNT' or
071100050330     C                             %subst(wNomeFile:1:3) = 'TM9' or
071200050330     C                             %subst(wNomeFile:1:3) = 'TMB' or
071300050330     C                             %subst(wNomeFile:1:3) = 'TOA' or
071400050330     C                             %subst(wNomeFile:1:3) = 'TOB'
071500050628     C                   eval      wCCM = 0061602
071600050330     C                   endif
071700050330     C*
071800050330     C                   if        %subst(wNomeFile:1:3) = 'TMP' or
071900050330     C                             %subst(wNomeFile:1:3) = 'TCF'
072000050628     C                   eval      wCCM = 0061602
072100050330     C                   endif
072200050330     C*
072300050330     C                   if        %subst(wNomeFile:1:3) = 'TRI'
072400050628     C                   eval      wCCM = 0061602
072500050330     C                   endif
072600050330     C*
072700050330     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
072800050330     C* del codice CMR dal nome del file importato.
072900050330     C                   if        wNomeFile <> *blanks AND
073000050330     C                             vlrppt<>*blanks
073100050330     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
073200050330     C                   if        %subst(vlrppt:1:1) = 'S'
073300050330     C                   seton                                        50
073400050330     C                   else
073500050330     C                   setoff                                       50
073600050330     C                   endif
073700050330     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
073800050330     C                   if        %subst(vlrppt:6:1) = 'S'
073900050330     C                   seton                                        51
074000050330     C                   else
074100050330     C                   setoff                                       51
074200050330     C                   endif
074300050330     C*
074400050330     C* Verifico se sui parametri del traduttore è richiesto il reperimento provincia
074500050330     C* (naturalmente solo se nn già presente)
074600050330     C                   if        %subst(vlrppt:7:1) = 'S'
074700050330     C                   seton                                        52
074800050330     C                   else
074900050330     C                   setoff                                       52
075000050330     C                   endif
075100050330     C*
075200050330     C                   eval      wPosDaA = %subst(vlrppt:2:2)
075300050330     C                   move(p)   wPosDaA       wPosDa
075400050330     C                   eval      wLungA  = %subst(vlrppt:4:2)
075500050330     C                   move(p)   wLungA        wLung
075600050330     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
075700050330     C                   seton                                        60
075800050330     C                   else
075900050330     C                   setoff                                       60
076000050330     C                   endif
076100010330     C*
076200010330     C* CALCOLA LA DATA CORRENTE
076300010330     C                   time                    wn14             14 0
076400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
076500010330     C                   z-add     wn8           g08dat
076600010330     C                   z-add     *zeros        g08inv
076700010330     C                   movel     '0'           g08err
076800010330     C                   call      'XSRDA8'
076900010330     C                   parm                    wlbda8
077000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
077100000613     C*
077200000613     C                   ENDSR
077300000613     C***
077400990908
077500050330     Otitvl7p   E            riepilogo         2
077600990915     O                                              'Upload via Internet'
077700990915     O                                           +1 'Traduzione TIVIN00R -'
077800050330     O                                           +1 'EDIVABWR'
077900990915     O                                           +1 'Testate bolle'
078000990915     O          E            riepilogo   2
078100990915     O                       wrkdata
078200990915     O                       wrkora              +1
078300990915     O                       procname            +1
078400990915     O          E            riepilogo   2
078500990915     O                                              'Cliente..................:'
078600010605     O                       VABCCM        z     +1
078700990915     O          E            riepilogo   2
078800990920     O                                              'Riferimento Strategi.....:'
078900990920     O                       vlrhdl              +1
079000990915     O          E            riepilogo   2
079100990915     O                                              'Giusti...................:'
079200971022     O                       §CTROK        2   +  1
079300990915     O          E            riepilogo   2
079400990915     O                                              'Sbagliati e corretti.....:'
079500971022     O                       §CTRMO        2   +  1
079600990915     O          E            riepilogo   2
079700990915     O                                              'Sbagliati e scartati.....:'
079800971022     O                       §CTRNO        2   +  1
079900000613
080000050330     Otitvl7ps  E            testdett          2
080100000613     O                                              'Upload via Internet'
080200000613     O                                           +1 'Traduzione TIVIN00R -'
080300050330     O                                           +1 'EDIVABWR'
080400000613     O                                           +1 'Testate bolle'
080500000616     O          E            testdett    3
080600000613     O                                           +2 'N° rec'
080700000613     O                                           +3 'Anteprima contenuto'
080800000616     O          E            rigadett    2
080900000613     O                       rrnum               +2
081000000621     O                       recko               +3
081100000616     O          E            findett     2
081200000613     O                       wrkdata
081300000613     O                       wrkora              +1
081400000613     O                       procname            +1
081500000616     O          E            findett     2
081600000613     O                                              'Cliente..................:'
081700010605     O                       VABCCM        z     +1
081800000616     O          E            findett     2
081900000613     O                                              'Riferimento Strategi.....:'
082000000613     O                       vlrhdl              +1
082100000616     O          E            findett     2
082200000613     O                                              'Giusti...................:'
082300000613     O                       §CTROK        2   +  1
082400000616     O          E            findett     2
082500000613     O                                              'Sbagliati e corretti.....:'
082600000613     O                       §CTRMO        2   +  1
082700000616     O          E            findett     2
082800000613     O                                              'Sbagliati e scartati.....:'
082900000613     O                       §CTRNO        2   +  1
083000000613** CMD - COMANDI CL
083100050330OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
083200050330OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVL7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
083300050330DLTOVR FILE(TITVL7P TITVL7PS) LVL(*)
083400010606
083500000613
