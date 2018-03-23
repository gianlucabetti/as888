000100050408      /TITLE Upload via Internet: traduzione in EDIVABWR. (x FABER  Cod. 0660701)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FEDIVABwr  O    E             DISK    usropn
000700050408     FEDIVATwr  O    E             DISK    usropn
000800050217     Ftitvi1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000050217     Ftitvi1ps  O    f  198        PRINTER usropn
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
003300050217     D parccm          s              8    INZ(*blanks)
003400050217     D parmbr          s             10    INZ(*blanks)
003500050217     D paropz          s              1    INZ(*blanks)
003600050217     D chkcall         s              1    INZ(*blanks)
003700050217     D wNOTE           s            100    INZ(*blanks)
003800010330     D*------------------
003900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004000010330     D*------------------
004100010330     D WLBDA8          DS                  INZ
004200010330     D  G08DAT                 1      8  0
004300010330     D  G08INV                 9     16  0
004400010330     D  G08ERR                17     17
004500010330     D  G08TGI                18     22  0
004600000313     D*
004700010330
004800010330
004900990915     C                   time                    wrkdata
005000990915     C                   time                    wrkora
005100000913     C                   reset                   rrnum
005200990921     C                   reset                   esito
005300990921     C                   reset                   wrkesito
005400010601     C*
005500010601     C                   exsr      opeini
005600010605     C                   exsr      rwvab
005700050217     C*
005800050217     C* Effettuo la chiamata al CLLE preposto
005900050412     C                   call(e)   'TITVEVTC'
006000050217     C                   parm                    parccm
006100050217     C                   parm                    parmbr
006200050217     C                   parm      '2'           paropz
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000000724     C*
008100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300010601     C     invfil        chain     azorg01l
008400000724     C                   if        %found
008500000724     C                   movel(p)  CMD(1)        depcmd
008600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008700000724     C*
008800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000724     C                   PARM                    depcmd
009200000724     C                   PARM                    LENGH
009300000724     C*
009400000724     C                   endif
009500000616     C*
009600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009700000616     C                   movel(p)  CMD(2)        depcmd
009800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000613     C*
010600050217     C                   if        not %open(titvi1ps)
010700050217     C                   open      titvi1ps
010800000616     C                   except    testdett
010900000613     C                   endif
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200990915     C*
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500050217     C                   if        %open(titvi1ps)
012600000616     C                   except    findett
012700050217     C                   close     titvi1ps
012800000613     C                   endif
012900000616     C*
013000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013100011113     C                   Z-ADD     150           LENGH            15 5
013200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    CMD(3)
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000000613
014100000613     C*--------------------------------------------------------
014200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014300000613     C*--------------------------------------------------------
014400000621     C     STPR          BEGSR
014500000613     C*
014600050217     C                   if        not %open(titvi1p)
014700050217     C                   open      titvi1p
014800990915     C                   endif
014900990915     C*
015000990915     C                   except    riepilogo
015100990915     C*
015200050217     C                   if        %open(titvi1p)
015300050217     C                   close     titvi1p
015400990914     C                   endif
015500000613     C*
015600000613     C                   ENDSR
015700000613     C***
015800000613
015900010601
016000010601
016100010330     C*--------------------------------------------------------
016200050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVAB0F              *
016300010330     C*--------------------------------------------------------
016400010605     C     RWVAB         BEGSR
016500010330     C*
016600010330     C                   if        not %open(tivin00r)
016700010330     C                   open      tivin00r
016800010330     C                   endif
016900050408     C                   if        not %open(edivabwr)
017000050408     C                   open      edivabwr
017100010330     C                   endif
017200050217     C*
017300050408     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
017400050217     C                   exsr      prevat
017500050217     C*
017600050217     C                   if        chkcall = '0'
017700050217     C*
017800050408     C                   if        not %open(edivatwr)
017900050408     C                   open      edivatwr
018000050217     C                   endif
018100010330     C*
018200010604     C                   clear                   §CTROK
018300010604     C                   clear                   §CTRMO
018400010604     C                   clear                   §CTRNO
018500010330     C*
018600010330     C                   DO        *HIVAL
018700010330     C*
018800010330     C                   READ      tivin00r                               70
018900010618     C*
019000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019100010618     C                   if        vinflg = '1'
019200010618     C                   eval      flgOk = '1'
019300010618     C                   endif
019400010618     C*
019500010330     C                   if        vindta > *blanks
019600010330     C                   add       1             rrnum
019700010330     C*
019800010601     C                   if        *in70 = *off and
019900010330     C                             (vinflg = *blanks
020000010330     C                              or vinflg = '0'
020100010330     C                              or vinflg = '2')
020200050217     C*
020300050408     C                   clear                   EDIVAB00
020400050408     C                   clear                   EDIVAT00
020500010330     C*
020600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020700010711     C                   if        vinflg = *blanks or vinflg = '0'
020800010711     C                   clear                   vinmsg
020900010711     C                   endif
021000010601     C*
021100010330     C                   exsr      inzvar
021200010330     C                   exsr      defcam
021300010605     C                   exsr      impvab
021400010601     C*
021500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021600010605     C                   eval      depfil = VABLNP
021700010601     C                   exsr      repfil
021800010601     C                   if        depfil = invfil
021900021025     C                   if        vlrpoi = 999
022000021025     C                   MOVE(P)   invfil        VABFGS
022100021025     C                   else
022200021025     C                   MOVE(P)   vlrpoi        VABFGS
022300021025     C                   endif
022400050114     C                   endif
022500010601     C*
022600010601     C                   exsr      PREELA
022700050408     C*
022800050408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
022900050408     C                   EVAL      VABCMR = 'FABER'
023000050408     C                   EVAL      VABDCM = DATCOR
023100050408     C                   EVAL      VABDTS = DATCOR
023200050408     C                   EVAL      VABHMS = ORACOR
023300050408     C                   EVAL      VABCNT = 1
023400050408     C*
023500050408     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
023600050408     C                   EVAL      VATCMR = 'FABER'
023700050408     C                   EVAL      VATCNT = 1
023800010601     C*
023900010604     C* Ebbene...
024000010604     C*
024100010604     C  N31              ADD       1             §CTROK            7 0
024200010604     C   32              ADD       1             §CTRMO            7 0
024300010604     C   31              ADD       1             §CTRNO            7 0
024400020205     C*
024500050408     C* Scarico il buffer del EDIVAB
024600050408     C  N31              WRITE     EDIVAB00
024700050217     C  N31              EXSR      WRIVAT
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
025900010604     C*
026000010330     C                   else
026100010330     C                   eval      vinflg = '1'
026200010330     C                   endif
026300010601     C*
026400010601     C  N70              update    tivin000
026500010330     C*
026600010330     C  N70              ENDdo
026700010601     C
026800010601     C*
026900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027000010601     C                   if        cntNonEl = *zeros or
027100010601     C                             flgMulti = '0'
027200010330     C* Se non ci sono record con errori ...
027300010601     C                   if        §ctrno = 0 and
027400010604     C                             §ctrmo = 0 and
027500010601     C                             flgStato <> '2'
027600010330     C* ... restituisco esito OK.
027700010330     C                   eval      wrkesito = '0'
027800010330     C                   else
027900010330     C                   if        §ctrok > 0
028000010330     C                   eval      wrkesito = '1'
028100010330     C                   else
028200010615     C                   if        flgOk = '0'
028300010615     C                   eval      wrkesito = '2'
028400010615     C                   else
028500010615     C                   eval      wrkesito = '6'
028600010615     C                   endif
028700010330     C                   endif
028800010330     C                   endif
028900010601     C                   else
029000010601     C                   eval      wrkesito = '9'
029100010601     C                   endif
029200050217     C*
029300050217     C                   else
029400050217     C                   eval      wrkesito = '2'
029500050217     C                   endif
029600010330     C*
029700010330     C                   if        %open(tivin00r)
029800010330     C                   close     tivin00r
029900010330     C                   endif
030000050408     C                   if        %open(edivabwr)
030100050408     C                   close     edivabwr
030200010330     C                   endif
030300050408     C                   if        %open(edivatwr)
030400050408     C                   close     edivatwr
030500050217     C                   endif
030600010601     C*
030700010601     C                   if        vlrpoi <> 999
030800010601     C                   eval      invfil = vlrpoi
030900010601     C                   endif
031000010330     C*
031100010330     C                   if        §ctrok > 0
031200010601     C                             and invfil > *zeros
031300010330     C                   exsr      invio
031400010330     C                   endif
031500010601     C*
031600010618     C                   if        flgGiro = '1'
031700010601     C                   exsr      endela
031800010618     C                   endif
031900010330     C*
032000010330     C                   ENDSR
032100010330     C***
032200010330
032300010601
032400010601
032500010601
032600010601
032700010330     C*----------------------------------------------------*
032800020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032900010330     C*----------------------------------------------------*
033000010330     C     INZVAR        BEGSR
033100010330     C*
033200020204     C                   Z-ADD     *zeros        Num5_0            5 0
033300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033400020322     C                   MOVEL     '0'           FlgCAS            1
033500010330     C*
033600010330     C                   ENDSR
033700010330     C*----------------------------------------------------*
033800020722     C*  IMPOSTAZIONE CAMPI COSTANTI
033900010330     C*----------------------------------------------------*
034000010330     C     DEFCAM        BEGSR
034100010330     C*
034200020204     C* Inizializzo il buffer del record da scrivere
034300050408     C                   CLEAR                   EDIVAT00
034400020204     C* Imposto i valori di default...
034500050217     C                   EVAL      VABCCM = 0660701
034600050217     C                   EVAL      VABLNP = 066
034700040713     C                   EVAL      VABCTR = 000
034800041108     C                   EVAL      VABCBO = '1'
034900050217     C                   EVAL      VABCTM = '7Q'
035000020204     C* ... e poi verifico se sono stati passati come parametri
035100020204     C                   IF        vlrppt > *blanks
035200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
035300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035400020204     C                   EXSR      CHKNUM
035500020204     C                   IF        PiInt=*on
035600020204     C                   Z-ADD     PiVal         VABCCM
035700020204     C                   ENDIF
035800040301     C                   ENDIF
035900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
036000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036100020204     C                   EXSR      CHKNUM
036200020204     C                   IF        PiInt=*on
036300020204     C                   Z-ADD     PiVal         VABLNP
036400020204     C                   ENDIF
036500040301     C                   ENDIF
036600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
036700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036800020204     C                   EXSR      CHKNUM
036900020204     C                   IF        PiInt=*on
037000020204     C                   Z-ADD     PiVal         VABCTR
037100040301     C                   ENDIF
037200020204     C                   ENDIF
037300020204     C                   ENDIF
037400020204     C*
037500010330     C                   ENDSR
037600010607     C*----------------------------------------------------*
037700050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
037800010607     C*----------------------------------------------------*
037900010607     C     IMPVAB        BEGSR
038000010607     C*
038100010607     C                   SETOFF                                       3132
038200010607     C*
038300010607     C* Reperimento campi ALFA
038400010607     C*
038500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
038600050217     C                   EVAL      VABRSD=%trim(%subst(vindta:103:30))
038700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
038900020117     C* ==
039000050217     C                   EVAL      VABIND=%trim(%subst(vindta:185:60))+' '+
039100050217     C                                    %trim(%subst(vindta:245:10))
039200050217     C                   EVAL      VABLOD=%trim(%subst(vindta:133:40))
039300050217     C                   EVAL      VABPRD=%trim(%subst(vindta:258:3))
039400080825     C                   EVAL      VABRMA=%trim(%subst(vindta:36:10))
039500050217     C                   EVAL      wNOTE =%trim(%subst(vindta:400:100))
039600050217     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
039700050217     C                   EVAL      VABNT2=%subst(wNOTE:1+35:35)
039800010607     C*
039900010607     C* Reperimento campi NUMERICI
040000020722     C                   MOVEL     DATCOR        VABAAS
040100020722     C                   MOVE      DATCOR        VABMGS
040200040420     C* NSP/RMN
040300080825     C                   EVAL      PiStr=%trim(%subst(vindta:36:10))
040400020304     C                   EXSR      CHKNUM
040500020304     C                   IF        PiInt=*on
040600040301     C                   Z-ADD     PiVal         VABNSP
040700040301     C                   Z-ADD     PiVal         VABRMN
040800020304     C                   ELSE
040900050217     C                   SETON                                        31
041000050217     C                   Z-ADD     *zeros        VABNSP
041100050217     C                   Z-ADD     *zeros        VABRMN
041200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
041300040301     C                             + ' ' + 'VABNSP VABRMN'
041400020304     C                   ENDIF
041500050128     C* CAD
041600050217     C                   EVAL      PiStr=%trim(%subst(vindta:173:12))
041700050128     C                   EXSR      CHKNUM
041800050128     C                   IF        PiInt=*on
041900050128     C                   Z-ADD     PiVal         Num5_0
042000050128     C                   MOVEL(p)  Num5_0        VABCAD
042100050128     C                   ELSE
042200050128     C                   SETON                                        32
042300050128     C                   EVAL      VABCAD = *zeros
042400050128     C                   EVAL      vinmsg = %trimr(vinmsg)
042500050128     C                             + ' ' + 'VABCAD'
042600050128     C                   ENDIF
042700040420     C* NCL
042800050217     C                   EVAL      PiStr=%trim(%subst(vindta:88:5))
042900010607     C                   EXSR      CHKNUM
043000010607     C                   IF        PiInt=*on
043100010607     C                   Z-ADD     PiVal         VABNCL
043200010607     C                   ELSE
043300010607     C                   SETON                                        32
043400010607     C                   Z-ADD     *zeros        VABNCL
043500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043600010607     C                             + ' ' + 'VABNCL'
043700010607     C                   ENDIF
043800090210     C* CTR
043900090210     C                   IF        %subst(vindta:500:3) = '001'
044000090210     C                   EVAL      PiStr=%trim(%subst(vindta:500:3))
044100090210     C                   EXSR      CHKNUM
044200090210     C                   IF        PiInt=*on
044300090210     C                   Z-ADD     PiVal         VABCTR
044400090210     C                   ELSE
044500090210     C                   SETON                                        32
044600090210     C                   Z-ADD     *zeros        VABCTR
044700090210     C                   EVAL      vinmsg = %trimr(vinmsg)
044800090210     C                             + ' ' + 'VABCTR'
044900090210     C                   ENDIF
045000090210     C                   ENDIF
045100040420     C* PKB
045200050217     C                   EVAL      PiStr=%trim(%subst(vindta:321:15))
045300010607     C                   EXSR      CHKNUM
045400010607     C                   IF        PiNum=*on
045500050114     C                   Z-ADD     PiVal         VABPKB
045600010607     C                   ELSE
045700010607     C                   SETON                                        32
045800010607     C                   Z-ADD     *zeros        VABPKB
045900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
046000010607     C                             + ' ' + 'VABPKB'
046100010607     C                   ENDIF
046200091124     C* VLB
046300091124     C                   EVAL      PiStr=%trim(%subst(vindta:354:15))
046400091124     C                   EXSR      CHKNUM
046500091124     C                   IF        PiNum=*on
046600091124     C                   Z-ADD     PiVal         VABVLB
046700091124     C                   ELSE
046800091124     C                   SETON                                        32
046900091124     C                   Z-ADD     *zeros        VABVLB
047000091124     C                   EVAL      vinmsg = %trimr(vinmsg)
047100091124     C                             + ' ' + 'VABVLB'
047200091124     C                   ENDIF
047300050216     C* CAS
047400050217     C                   IF        %trim(%subst(vindta:372:15)) <> '0,00'
047500050216     C                   MOVEL     '1'           FlgCAS
047600050217     C                   EVAL      PiStr=%trim(%subst(vindta:372:15))
047700050217     C                   EVAL      VABVCA='EUR'
047800050216     C                   EXSR      CHKNUM
047900050216     C                   IF        PiNum=*on
048000050216     C                   Z-ADD     PiVal         VABCAS
048100050216     C                   ELSE
048200050216     C                   SETON                                        32
048300050216     C                   EVAL      vinmsg = %trimr(vinmsg)
048400050216     C                             + ' ' + 'VABCAS'
048500050216     C                   ENDIF
048600050216     C                   ENDIF
048700020322     C*
048800020322     C* Considerazioni finali su CBO/CAS
048900020322     C                   IF        FlgCAS = '1'
049000040713     C                   IF        VABCBO = '1'
049100020322     C                   EVAL      VABCBO = '4'
049200040713     C                   ENDIF
049300040713     C                   IF        VABCBO = '2'
049400040713     C                   EVAL      VABCBO = '6'
049500040713     C                   ENDIF
049600020322     C                   ENDIF
049700020204     C*
049800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049900020204     C                   EXSR      CHKIMPDIV
050000020204     C*
050100010607     C                   ENDSR
050200010607     C*----------------------------------------------------*
050300010601
050400020204
050500020204     C*----------------------------------------------------*
050600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050700020204     C*----------------------------------------------------*
050800020204     C     CHKIMPDIV     BEGSR
050900020204     C*
051000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
051100020204     C                   Z-ADD     *zeros        wrkDec            9 9
051200020204     C*
051300020204     C* Come prima cosa effettuo considerazioni sulla divisa
051400020204     C                   IF        vabIAS > *zeros
051500020204     C                   IF        vabVAS <> 'EUR'
051600020204     C                   EVAL      vabVAS =  'ITL'
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C                   IF        vabCAS > *zeros
052100020204     C                   IF        vabVCA <> 'EUR'
052200020204     C                   EVAL      vabVCA =  'ITL'
052300020204     C                   ENDIF
052400020204     C                   ENDIF
052500020204     C*
052600020204     C                   IF        vabVMD > *zeros
052700020321     C                   IF        vabVAD <> 'EUR'
052800020204     C                   EVAL      vabVAD =  'ITL'
052900020204     C                   ENDIF
053000020204     C                   ENDIF
053100020204     C*
053200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
053300020204     C                   Z-ADD     vabIAS        wrkDec
053400020204     C                   IF        wrkDec > *zeros
053500020204     C                   IF        vabVAS = 'ITL'
053600020204     C                   EVAL      vabIAS = *zeros
053700020204     C                   ENDIF
053800020204     C                   ENDIF
053900020204     C*
054000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
054100020204     C                   Z-ADD     vabCAS        wrkDec
054200020204     C                   IF        wrkDec > *zeros
054300020204     C                   IF        vabVCA = 'ITL'
054400020204     C                   EVAL      vabCAS = *zeros
054500020204     C                   ENDIF
054600020204     C                   ENDIF
054700020204     C*
054800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054900020204     C                   Z-ADD     vabVMD        wrkDec
055000020204     C                   IF        wrkDec > *zeros
055100020204     C                   IF        vabVAD = 'ITL'
055200020204     C                   EVAL      vabVMD = *zeros
055300020204     C                   ENDIF
055400020204     C                   ENDIF
055500020204     C*
055600020204     C                   ENDSR
055700020204     C***
055800020204
055900010330
056000010330
056100010330     C*----------------------------------------------------*
056200010330     C*  CONTROLLO NUMERICITA' CAMPI
056300010330     C*----------------------------------------------------*
056400010330     C     CHKNUM        BEGSR
056500010330     C*
056600010606     C                   IF        PiDecChr = *blanks
056700050217     C                   EVAL      PiDecChr = ','
056800010606     C                   ENDIF
056900010606     C*
057000010606     C                   CALL(e)   'ISNUMERIC'
057100010330     C                   PARM                    PiStr            30
057200010606     C                   PARM                    PiDecChr          1
057300010330     C                   PARM      *ZEROS        PiVal            30 9
057400010330     C                   PARM      '0'           PiInt             1
057500010330     C                   PARM      '0'           PiNum             1
057600010330     C                   IF        %error
057700010606     C                   EVAL      PiNum=*off
057800010330     C                   ENDIF
057900010330     C*
058000010330     C                   ENDSR
058100010330     C***
058200050217
058300050217
058400050217
058500050217     C*----------------------------------------------------*
058600050408     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
058700050217     C*----------------------------------------------------*
058800050217     C     PREVAT        BEGSR
058900050217     C*
059000050408     C* Compongo il nome del membro da dare al EDIVATWR
059100050217     C                   eval      parmbr = vlrhdl
059200050217     C                   movel     'M'           parmbr
059300050217     C                   eval      parccm = %subst(vlrKSC:2:7)
059400050217     C                   eval      paropz = '1'
059500050217     C* Effettuo la chiamata al CLLE preposto
059600050412     C                   call(e)   'TITVEVTC'
059700050217     C                   parm                    parccm
059800050217     C                   parm                    parmbr
059900050217     C                   parm                    paropz
060000050217     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060100050217     C                   if        %error
060200050217     C                   movel     '1'           chkcall
060300050217     C                   else
060400050217     C                   movel     '0'           chkcall
060500050217     C                   endif
060600050217     C*
060700050217     C                   ENDSR
060800050217
060900050217
061000050217
061100050217     C*----------------------------------------------------*
061200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
061300050217     C*----------------------------------------------------*
061400050217     C     WRIVAT        BEGSR
061500050217     C*
061600050217     C* Valorizzo il buffer del file dettagli barcode colli
061700050217     C                   EVAL      VATFGS = VABFGS
061800050217     C                   EVAL      VATAAS = VABAAS
061900050217     C                   EVAL      VATLNP = VABLNP
062000050217     C                   EVAL      VATNSP = VABNSP
062100050217     C                   EVAL      VATCCM = VABCCM
062200050217     C                   EVAL      VATTRC='B'
062300050217     C                   EVAL      VATNOT = %trim(%subst(vindta:261:30))
062400050217     C                   IF        VATNOT <> *blanks
062500050408     C                   write     EDIVAT00
062600050217     C                   ENDIF
062700050217     C*
062800050217     C                   ENDSR
062900010330
063000010601
063100010601
063200010601
063300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
063400010601     C     repfil        BEGSR
063500010601     C*
063600010601     C                   if        invfil = *zeros and
063700010601     C                             depfil > *zeros and
063800010629     C                             (vinflg = *blanks or
063900010629     C                              vinflg = *zeros)
064000010601     C
064100010601     C                   eval      invfil = depfil
064200010601     C                   endif
064300010601     C*
064400010601     C                   if        depfil <> invfil and
064500010601     C                             invfil > *zeros
064600010601     C                   eval      flgMulti = '1'
064700010601     C                   if        vinflg = *blanks
064800010601     C                   add       1             cntNonEl
064900010601     C                   endif
065000010601     C                   endif
065100010601     C*
065200010601     C                   if        vinflg = '2'
065300010601     C                   eval      flgStato = '2'
065400010601     C                   endif
065500010601     C*
065600010601     C                   ENDSR
065700010601     C***
065800050217
065900050217
066000050217
066100050217
066200050217      /TITLE Invio dei dati al punto operativo.
066300050217     C     invio         BEGSR
066400050217     C*
066500050408     C* 1° invio EDIVAT
066600050217     C                   reset                   dscmz
066700050217     C                   move      vlrpoi        cmzdst
066800050408     C                   eval      cmzfld = 'EDIVATWR'
066900050412     C                   eval      cmzmbd = parmbr
067000050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
067100050217     C***                if        prmfir = *blanks
067200050408     C                   eval      cmzfla = 'EDIVAT0F'
067300050408     C                   eval      cmzmba = 'EDIVAT0F'
067400050217     C***                else
067500050217     C***                eval      cmzfla = prmfir
067600050217     C***                eval      cmzmba = prmfir
067700050217     C***                endif
067800050217     C                   eval      cmznrr = *zeros
067900050217     C                   move      §ctrok        cmznrr
068000050217     C                   eval      cmzlba = vlrfl1
068100050217     C                   call(e)   'TIS711C'
068200050217     C                   parm                    dscmz
068300050217     C                   parm      *blanks       esito
068400050217     C                   if        %error
068500050217     C                             or cmzerr = '1'
068600050217     C                             or esito  = '1'
068700050217     C                   eval      wrkesito = '3'
068800050217     C                   else
068900050217     C*
069000050408     C* 2° invio EDIVAB
069100050217     C                   reset                   dscmz
069200050217     C                   move      vlrpoi        cmzdst
069300050217     C                   eval      cmzfld = vlrfou
069400050217     C                   eval      cmzmbd = vlrhdl
069500050217     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600050217     C***                if        prmfir = *blanks
069700050408     C                   eval      cmzfla = 'EDIVAB0F'
069800050408     C                   eval      cmzmba = 'EDIVAB0F'
069900050217     C***                else
070000050217     C***                eval      cmzfla = prmfir
070100050217     C***                eval      cmzmba = prmfir
070200050217     C***                endif
070300050217     C                   eval      cmznrr = *zeros
070400050217     C                   move      §ctrok        cmznrr
070500050217     C                   eval      cmzlba = vlrfl1
070600050217     C                   call(e)   'TIS711C'
070700050217     C                   parm                    dscmz
070800050217     C                   parm      *blanks       esito
070900050217     C                   if        %error
071000050217     C                             or cmzerr = '1'
071100050217     C                             or esito  = '1'
071200050217     C                   eval      wrkesito = '3'
071300050217     C                   endif
071400050217     C                   endif
071500050217     C*
071600050217     C                   ENDSR
071700050217     C***
071800050217
071900050217
072000050217
072100010601
072200010601
072300010601      /TITLE Invio dei dati al punto operativo.
072400010601     C     opeini        BEGSR
072500010601     C*
072600010601     C* Inizializzo flag e contatori operativi
072700010601     C                   movel     '0'           flgGiro           1
072800010601     C                   movel     '0'           flgMulti          1
072900010601     C                   movel     '1'           flgStato          1
073000010615     C                   movel     '0'           flgOk             1
073100010601     C                   z-add     *zeros        cntNonEl         10 0
073200010601     C                   z-add     *zeros        depfil            3 0
073300010601     C                   z-add     *zeros        invfil            3 0
073400010601     C*
073500010601     C                   ENDSR
073600010601     C***
073700010601
073800010601
073900010601
074000010330
074100010330
074200000613     C     *inzsr        BEGSR
074300990910     C*
074400990910     C     *entry        plist
074500990920     C                   parm                    tivlrds
074600990921     C                   parm      wrkesito      esito
074700000724     C                   parm                    prmlit
074800000710     C                   parm                    prmfir
074900010330     C*
075000010330     C* CALCOLA LA DATA CORRENTE
075100010330     C                   time                    wn14             14 0
075200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075300050408     C                   movel     wn14          oracor            6 0          *ORA
075400010330     C                   z-add     wn8           g08dat
075500010330     C                   z-add     *zeros        g08inv
075600010330     C                   movel     '0'           g08err
075700010330     C                   call      'XSRDA8'
075800010330     C                   parm                    wlbda8
075900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076000000613     C*
076100000613     C                   ENDSR
076200000613     C***
076300990908
076400050217     Otitvi1p   E            riepilogo         2
076500990915     O                                              'Upload via Internet'
076600990915     O                                           +1 'Traduzione TIVIN00R -'
076700050408     O                                           +1 'EDIVABWR'
076800990915     O                                           +1 'Testate bolle'
076900990915     O          E            riepilogo   2
077000990915     O                       wrkdata
077100990915     O                       wrkora              +1
077200990915     O                       procname            +1
077300990915     O          E            riepilogo   2
077400990915     O                                              'Cliente..................:'
077500010605     O                       VABCCM        z     +1
077600990915     O          E            riepilogo   2
077700990920     O                                              'Riferimento Strategi.....:'
077800990920     O                       vlrhdl              +1
077900990915     O          E            riepilogo   2
078000990915     O                                              'Giusti...................:'
078100971022     O                       §CTROK        2   +  1
078200990915     O          E            riepilogo   2
078300990915     O                                              'Sbagliati e corretti.....:'
078400971022     O                       §CTRMO        2   +  1
078500990915     O          E            riepilogo   2
078600990915     O                                              'Sbagliati e scartati.....:'
078700971022     O                       §CTRNO        2   +  1
078800000613
078900050217     Otitvi1ps  E            testdett          2
079000000613     O                                              'Upload via Internet'
079100000613     O                                           +1 'Traduzione TIVIN00R -'
079200050408     O                                           +1 'EDIVABWR'
079300000613     O                                           +1 'Testate bolle'
079400000616     O          E            testdett    3
079500000613     O                                           +2 'N° rec'
079600000613     O                                           +3 'Anteprima contenuto'
079700000616     O          E            rigadett    2
079800000613     O                       rrnum               +2
079900000621     O                       recko               +3
080000000616     O          E            findett     2
080100000613     O                       wrkdata
080200000613     O                       wrkora              +1
080300000613     O                       procname            +1
080400000616     O          E            findett     2
080500000613     O                                              'Cliente..................:'
080600010605     O                       VABCCM        z     +1
080700000616     O          E            findett     2
080800000613     O                                              'Riferimento Strategi.....:'
080900000613     O                       vlrhdl              +1
081000000616     O          E            findett     2
081100000613     O                                              'Giusti...................:'
081200000613     O                       §CTROK        2   +  1
081300000616     O          E            findett     2
081400000613     O                                              'Sbagliati e corretti.....:'
081500000613     O                       §CTRMO        2   +  1
081600000616     O          E            findett     2
081700000613     O                                              'Sbagliati e scartati.....:'
081800000613     O                       §CTRNO        2   +  1
081900000613** CMD - COMANDI CL
082000050217OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
082100050217OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVI1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
082200050217DLTOVR FILE(TITVI1P TITVI1PS) LVL(*)
082300010606
082400000613
