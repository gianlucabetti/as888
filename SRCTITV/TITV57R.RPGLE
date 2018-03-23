000100040628      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR. (x MEDIAWORLD - Cod. 0931845)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040628     FFIVATwwr  O    E             DISK    usropn
000800030606     Ftitv57p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000030606     Ftitv57ps  O    f  198        PRINTER usropn
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
003300040628     D parccm          s              8    INZ(*blanks)
003400040628     D parmbr          s             10    INZ(*blanks)
003500040628     D paropz          s              1    INZ(*blanks)
003600040628     D chkcall         s              1    INZ(*blanks)
003700060113     D wChiSono        s             12    VARYING INZ(*zeros)
003800060823     D wTelDest        s             30    VARYING INZ(*blanks)
003900010330     D*------------------
004000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100010330     D*------------------
004200010330     D WLBDA8          DS                  INZ
004300010330     D  G08DAT                 1      8  0
004400010330     D  G08INV                 9     16  0
004500010330     D  G08ERR                17     17
004600010330     D  G08TGI                18     22  0
004700000313     D*
004800010330
004900010330
005000010330
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600010601     C*
005700010601     C                   exsr      opeini
005800010605     C                   exsr      rwvab
005900040628     C*
006000040628     C* Effettuo la chiamata al CLLE preposto
006100040628     C                   call(e)   'TITVVTC'
006200040628     C                   parm                    parccm
006300040628     C                   parm                    parmbr
006400040628     C                   parm      '2'           paropz
006500010601     C*
006600010601     C                   seton                                        lr
006700010601
006800010601
006900010601
007000010601
007100010601     C*--------------------------------------------------------
007200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007300010601     C*--------------------------------------------------------
007400010601     C     PREELA        BEGSR
007500010601     C*
007600010601     C* SE OCCORRE SPEDIRE IN FILIALE
007700010601     C                   if        invfil <> *zeros and
007800010601     C                             flgGiro = '0'
007900010601     C*
008000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008100010601     C                   eval      flgGiro = '1'
008200000724     C*
008300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500010601     C     invfil        chain     azorg01l
008600000724     C                   if        %found
008700000724     C                   movel(p)  CMD(1)        depcmd
008800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008900000724     C*
009000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009100011113     C                   Z-ADD     150           LENGH            15 5
009200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009300000724     C                   PARM                    depcmd
009400000724     C                   PARM                    LENGH
009500000724     C*
009600000724     C                   endif
009700000616     C*
009800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009900000616     C                   movel(p)  CMD(2)        depcmd
010000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010100000616     C*
010200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010300011113     C                   Z-ADD     150           LENGH            15 5
010400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010500000616     C                   PARM                    depcmd
010600000616     C                   PARM                    LENGH
010700000613     C*
010800030606     C                   if        not %open(titv57ps)
010900030606     C                   open      titv57ps
011000000616     C                   except    testdett
011100000613     C                   endif
011200010601     C*
011300010601     C                   endif
011400010601     C*
011500010601     C                   ENDSR
011600010601     C***
011700010601
011800010601
011900010601
012000010601
012100010601
012200010601
012300010601
012400010601     C*--------------------------------------------------------
012500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012600010601     C*--------------------------------------------------------
012700010601     C     ENDELA        BEGSR
012800990915     C*
012900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013000000613     C*
013100030606     C                   if        %open(titv57ps)
013200000616     C                   except    findett
013300030606     C                   close     titv57ps
013400000613     C                   endif
013500000616     C*
013600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013700011113     C                   Z-ADD     150           LENGH            15 5
013800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013900000616     C                   PARM                    CMD(3)
014000000616     C                   PARM                    LENGH
014100000616     C*
014200010601     C                   ENDSR
014300010601     C***
014400010601
014500010601
014600010601
014700010601
014800000613
014900000613     C*--------------------------------------------------------
015000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015100000613     C*--------------------------------------------------------
015200000621     C     STPR          BEGSR
015300000613     C*
015400030606     C                   if        not %open(titv57p)
015500030606     C                   open      titv57p
015600990915     C                   endif
015700990915     C*
015800990915     C                   except    riepilogo
015900990915     C*
016000030606     C                   if        %open(titv57p)
016100030606     C                   close     titv57p
016200990914     C                   endif
016300000613     C*
016400000613     C                   ENDSR
016500000613     C***
016600000613
016700010330
016800010601
016900010601
017000010601
017100010330     C*--------------------------------------------------------
017200040628     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF e FIVATWWR   *
017300010330     C*--------------------------------------------------------
017400010605     C     RWVAB         BEGSR
017500010330     C*
017600010330     C                   if        not %open(tivin00r)
017700010330     C                   open      tivin00r
017800010330     C                   endif
017900021025     C                   if        not %open(fivabwwr)
018000021025     C                   open      fivabwwr
018100010330     C                   endif
018200040628     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018300040628     C                   exsr      prevat
018400040628     C*
018500040628     C                   if        chkcall = '0'
018600040628     C*
018700040628     C                   if        not %open(fivatwwr)
018800040628     C                   open      fivatwwr
018900040628     C                   endif
019000010330     C*
019100040628     C                   clear                   §CTROKVB
019200040628     C                   clear                   §CTROKVT
019300040628     C                   clear                   §CTRMO
019400010604     C                   clear                   §CTRNO
019500010330     C*
019600010330     C                   DO        *HIVAL
019700010330     C*
019800010330     C                   READ      tivin00r                               70
019900010618     C*
020000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020100010618     C                   if        vinflg = '1'
020200010618     C                   eval      flgOk = '1'
020300010618     C                   endif
020400010618     C*
020500010330     C                   if        vindta > *blanks
020600010330     C                   add       1             rrnum
020700010330     C*
020800010601     C                   if        *in70 = *off and
020900010330     C                             (vinflg = *blanks
021000010330     C                              or vinflg = '0'
021100010330     C                              or vinflg = '2')
021200010330     C*
021300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021400010711     C                   if        vinflg = *blanks or vinflg = '0'
021500010711     C                   clear                   vinmsg
021600010711     C                   endif
021700040628     C*
021800010330     C                   exsr      inzvar
021900010330     C                   exsr      defcam
022000010605     C                   exsr      impvab
022100010601     C*
022200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022300010605     C                   eval      depfil = VABLNP
022400010601     C                   exsr      repfil
022500010601     C                   if        depfil = invfil
022600021025     C                   if        vlrpoi = 999
022700021025     C                   MOVE(P)   invfil        VABFGS
022800021025     C                   else
022900021025     C                   MOVE(P)   vlrpoi        VABFGS
023000021025     C                   endif
023100040628     C*
023200040628     C                   exsr      wrivat
023300010601     C*
023400010601     C                   exsr      PREELA
023500010601     C*
023600010604     C* Ebbene...
023700010604     C*
023800040628     C  N31              ADD       1             §CTROKVB          7 0
023900010604     C   32              ADD       1             §CTRMO            7 0
024000010604     C   31              ADD       1             §CTRNO            7 0
024100021025     C  N31              WRITE     FIVAB000
024200010604     C*
024300010604     C                   if        *in31 = *off and
024400010604     C                             *in32 = *off
024500010604     C                   eval      vinflg = '1'
024600010604     C                   else
024700010604     C                   eval      recko = vindta
024800010604     C                   except    rigadett
024900010604     C                   eval      vinflg = '2'
025000010604     C                   endif
025100010604     C                   endif
025200010601     C*
025300010604     C                   endif
025400010604     C*
025500010330     C                   else
025600010330     C                   eval      vinflg = '1'
025700010330     C                   endif
025800010601     C*
025900010601     C  N70              update    tivin000
026000010330     C*
026100010330     C  N70              ENDdo
026200040628     C*
026300040628     C                   endif
026400010601     C*
026500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026600010601     C                   if        cntNonEl = *zeros or
026700010601     C                             flgMulti = '0'
026800010330     C* Se non ci sono record con errori ...
026900010601     C                   if        §ctrno = 0 and
027000010604     C                             §ctrmo = 0 and
027100010601     C                             flgStato <> '2'
027200010330     C* ... restituisco esito OK.
027300010330     C                   eval      wrkesito = '0'
027400010330     C                   else
027500040628     C                   if        §ctrokvb > 0
027600010330     C                   eval      wrkesito = '1'
027700010330     C                   else
027800010615     C                   if        flgOk = '0'
027900010615     C                   eval      wrkesito = '2'
028000010615     C                   else
028100010615     C                   eval      wrkesito = '6'
028200010615     C                   endif
028300010330     C                   endif
028400010330     C                   endif
028500010601     C                   else
028600010601     C                   eval      wrkesito = '9'
028700010601     C                   endif
028800010330     C*
028900010330     C                   if        %open(tivin00r)
029000010330     C                   close     tivin00r
029100010330     C                   endif
029200021025     C                   if        %open(fivabwwr)
029300021025     C                   close     fivabwwr
029400010330     C                   endif
029500040628     C                   if        %open(fivatwwr)
029600040628     C                   close     fivatwwr
029700040628     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300040628     C                   if        §ctrokvb > 0
030400010601     C                             and invfil > *zeros
030500010330     C                   exsr      invio
030600010330     C                   endif
030700010601     C*
030800010618     C                   if        flgGiro = '1'
030900010601     C                   exsr      endela
031000010618     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400010601
031500010601
031600010601
031700010330     C*----------------------------------------------------*
031800010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031900010330     C*----------------------------------------------------*
032000010330     C     INZVAR        BEGSR
032100010330     C*
032200020204     C                   Z-ADD     *zeros        Num5_0            5 0
032300030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032400030606     C                   MOVEL     '0'           FlgCAS            1
032500010330     C*
032600010330     C                   ENDSR
032700010330     C*----------------------------------------------------*
032800010330     C*  IMPOSTAZIONE CAMPI COSTANTI
032900010330     C*----------------------------------------------------*
033000010330     C     DEFCAM        BEGSR
033100010330     C*
033200020204     C* Inizializzo il buffer del record da scrivere
033300021025     C                   CLEAR                   FIVAB000
033400040628     C                   CLEAR                   FIVAT000
033500020204     C* Imposto i valori di default...
033600030606     C                   EVAL      VABCCM = 0931845
033700030606     C                   EVAL      VABLNP = 093
033800030526     C                   EVAL      VABCTR = 000
033900040628     C                   EVAL      VABCTM = '7Q'
033901080618     C                   EVAL      VABTSP = 'C'
034000020204     C* ... e poi verifico se sono stati passati come parametri
034100020204     C                   IF        vlrppt > *blanks
034200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034300020204     C                   EXSR      CHKNUM
034400020204     C                   IF        PiInt=*on
034500020204     C                   Z-ADD     PiVal         VABCCM
034600020204     C                   ENDIF
034700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034800020204     C                   EXSR      CHKNUM
034900020204     C                   IF        PiInt=*on
035000020204     C                   Z-ADD     PiVal         VABLNP
035100020204     C                   ENDIF
035200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035300020204     C                   EXSR      CHKNUM
035400020204     C                   IF        PiInt=*on
035500020204     C                   Z-ADD     PiVal         VABCTR
035600020204     C                   ENDIF
035700020204     C                   ENDIF
035800020204     C*
035900010330     C                   ENDSR
036000010607     C*----------------------------------------------------*
036100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036200010607     C*----------------------------------------------------*
036300010607     C     IMPVAB        BEGSR
036400010607     C*
036500010607     C                   SETOFF                                       3132
036600030515     C*
036700010607     C                   MOVEL     datcor        VABAAS
036800010607     C                   MOVE      datcor        VABMGS
036900010607     C*
037000010607     C* Reperimento campi ALFA
037100010607     C*
037200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
037300030606     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
037400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037600020117     C* ==
037700030606     C                   EVAL      VABRD2=%trim(%subst(vindta:17+35:40-35))+
037800030606     C                                    ' tel.'+%trim(%subst(vindta:92:15))
037900030606     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
038000030606     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
038100030606     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
038200030606     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
038300030606     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
038400030606     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
038500030606     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
038600030606     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
038700060823     C                   EVAL      wTelDest=%trim(%subst(vindta:92:15))
038800080618     C                   IF        %subst(vindta:12:1) = 'B'
038900050726     C                   EVAL      VABTC1='A'
039000050726     C                   ENDIF
039001080618     C                   IF        %subst(vindta:12:1) = 'F'
039002080618     C                   EVAL      VABTC1='P'
039003080618     C                   ENDIF
039100080618     C                   IF        %subst(vindta:12:1) = 'C'
039200050803     C                   EVAL      VABTSP='E'
039300050803     C                   ENDIF
039700010607     C*
039800010607     C* Reperimento campi NUMERICI
039900020604     C*
040000030515     C* CAD
040100030606     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
040200010607     C                   EXSR      CHKNUM
040300010607     C                   IF        PiInt=*on
040400020204     C                   Z-ADD     PiVal         Num5_0
040500020204     C                   MOVEL(p)  Num5_0        VABCAD
040600010607     C                   ELSE
040700010607     C                   SETON                                        32
040800020204     C                   EVAL      VABCAD = *zeros
040900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
041000020204     C                             + ' ' + 'VABCAD'
041100010607     C                   ENDIF
041200030515     C* RMN/NSP
041300030606     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
041400011023     C                   EXSR      CHKNUM
041500011023     C                   IF        PiInt=*on
041600030225     C                   Z-ADD     PiVal         VABRMN
041700030225     C                   Z-ADD     PiVal         VABNSP
041800011023     C                   ELSE
041900020205     C                   EVAL      VABRMN = 1
042000011023     C                   ENDIF
042100030515     C* NCL
042200030606     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
042300010607     C                   EXSR      CHKNUM
042400010607     C                   IF        PiInt=*on
042500010607     C                   Z-ADD     PiVal         VABNCL
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800010607     C                   Z-ADD     *zeros        VABNCL
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000010607     C                             + ' ' + 'VABNCL'
043100010607     C                   ENDIF
043200030515     C* PKB
043300030606     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
043400030606     C                   EVAL      PiDecChr = '.'
043500010607     C                   EXSR      CHKNUM
043600010607     C                   IF        PiNum=*on
043700010607     C                   Z-ADD(H)  PiVal         VABPKB
043800010607     C                   ELSE
043900010607     C                   SETON                                        32
044000010607     C                   Z-ADD     *zeros        VABPKB
044100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044200010607     C                             + ' ' + 'VABPKB'
044300010607     C                   ENDIF
044400030606     C* CAS
044500030606     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
044600030606     C                   IF        PiStr <> *blanks AND
044700030606     C                             PiStr <> *zeros
044800030606     C                   MOVEL     '1'           FlgCAS
044900030606     C                   EVAL      PiDecChr = ','
045000030606     C                   EXSR      CHKNUM
045100030606     C                   IF        PiNum=*on
045200030606     C                   Z-ADD     PiVal         VABCAS
045300030606     C                   ELSE
045400030606     C                   SETON                                        32
045500030606     C                   Z-ADD     *zeros        VABCAS
045600030606     C                   EVAL      vinmsg = %trimr(vinmsg)
045700030606     C                             + ' ' + 'VABCAS'
045800030606     C                   ENDIF
045900030606     C                   ENDIF
046000030606     C* IAS
046100030606     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
046200030606     C                   EVAL      PiDecChr = ','
046300030606     C                   EXSR      CHKNUM
046400030606     C                   IF        PiNum=*on
046500030606     C                   Z-ADD     PiVal         VABIAS
046600030606     C                   ELSE
046700030606     C                   SETON                                        32
046800030606     C                   Z-ADD     *zeros        VABIAS
046900030606     C                   EVAL      vinmsg = %trimr(vinmsg)
047000030606     C                             + ' ' + 'VABIAS'
047100030606     C                   ENDIF
047900030606     C*
048000030606     C* Considerazioni finali su CBO/CAS
048100030606     C                   IF        FlgCAS = '1'
048200030606     C                   EVAL      VABCBO = '4'
048300030606     C                   ELSE
048400030606     C                   EVAL      VABCBO = '1'
048500030606     C                   ENDIF
048600020204     C*
048700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048800020204     C                   EXSR      CHKIMPDIV
048900020204     C*
049000010607     C                   ENDSR
049100010607     C*----------------------------------------------------*
049200040628
049300040628
049400040628
049500040628     C*----------------------------------------------------*
049600040628     C*  WRIVAT - SCRITTURA RECORD DETTAGLIO COLLI DISK "C" - FILE FIVAT
049700040628     C*----------------------------------------------------*
049800040628     C     WRIVAT        BEGSR
049900040628     C*
050000040628     C                   EVAL      VATFGS = VABFGS
050100040628     C                   EVAL      VATCCM = VABCCM
050200040628     C                   EVAL      VATLNP = VABLNP
050300040628     C                   EVAL      VATAAS = VABAAS
050400040628     C                   EVAL      VATNSP = VABNSP
050500040628     C                   EVAL      VATTRC = 'E'
050600040628     C*
050700040628     C* X ogni collo della testata bolle scrivo 1 record d VAT
050800040628     C                   Z-ADD     1             wNCL              5 0
050900040628     C                   MOVEL     *blanks       wNCLALfa          4
051000040628     C                   DOW       wNCL <= VABNCL
051100040628     C                   MOVE(P)   wNCL          wNCLALfa
051200060113     C                   EVAL      wChiSono = %trim(VABRMA) + wNCLAlfa
051300060113     C                   MOVEL     *zeros        wVATNOT          12
051400060113     C                   MOVE      wChiSono      wVATNOT
051500060113     C                   EVAL      VATNOT = wVATNOT
051600040628     C                   WRITE(e)  FIVAT000
051700040628     C                   IF        not %error
051800040628     C                   ADD       1             §CTROKVT          7 0
051900040628     C                   ENDIF
052000040628     C                   ADD       1             wNCL
052100040628     C                   ENDDO
052200060823     C*
052300060823     C* Scrivo inoltre 1 record x il telefono destinatario (tipo record 'B')
052400060823     C                   EVAL      VATNOT = wTelDest
052500060823     C                   EVAL      VATTRC = 'B'
052600060823     C                   WRITE(e)  FIVAT000
052700060823     C                   IF        not %error
052800060823     C                   ADD       1             §CTROKVT          7 0
052900060823     C                   ENDIF
053000040628     C*
053100040628     C                   ENDSR
053200040628     C*----------------------------------------------------*
053300040628
053400040628
053500040628
053600040628     C*----------------------------------------------------*
053700040628     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053800040628     C*----------------------------------------------------*
053900040628     C     PREVAT        BEGSR
054000040628     C*
054100040628     C* Compongo il nome del membro da dare al FIVATWWR
054200040628     C                   eval      parmbr = vlrhdl
054300040628     C                   movel     'M'           parmbr
054400040628     C                   eval      parccm = '00931845'
054500040628     C                   eval      paropz = '1'
054600040628     C* Effettuo la chiamata al CLLE preposto
054700040628     C                   call(e)   'TITVVTC'
054800040628     C                   parm                    parccm
054900040628     C                   parm                    parmbr
055000040628     C                   parm                    paropz
055100040628     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055200040628     C                   if        %error
055300040628     C                   movel     '1'           chkcall
055400040628     C                   else
055500040628     C                   movel     '0'           chkcall
055600040628     C                   endif
055700040628     C*
055800040628     C                   ENDSR
055900010601
056000020204
056100020204     C*----------------------------------------------------*
056200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056300020204     C*----------------------------------------------------*
056400020204     C     CHKIMPDIV     BEGSR
056500020204     C*
056600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056700020204     C                   Z-ADD     *zeros        wrkDec            9 9
056800020204     C*
056900020204     C* Come prima cosa effettuo considerazioni sulla divisa
057000020204     C                   IF        vabIAS > *zeros
057100020204     C                   IF        vabVAS <> 'EUR'
057200020204     C                   EVAL      vabVAS =  'ITL'
057300020204     C                   ENDIF
057400020204     C                   ENDIF
057500020204     C*
057600020204     C                   IF        vabCAS > *zeros
057700020204     C                   IF        vabVCA <> 'EUR'
057800020204     C                   EVAL      vabVCA =  'ITL'
057900020204     C                   ENDIF
058000020204     C                   ENDIF
058100020204     C*
058200020204     C                   IF        vabVMD > *zeros
058300020204     C                   IF        vabVAD <> 'EUR'
058400020204     C                   EVAL      vabVAD =  'ITL'
058500020204     C                   ENDIF
058600020204     C                   ENDIF
058700020204     C*
058800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058900020204     C                   Z-ADD     vabIAS        wrkDec
059000020204     C                   IF        wrkDec > *zeros
059100020204     C                   IF        vabVAS = 'ITL'
059200020204     C                   EVAL      vabIAS = *zeros
059300020204     C                   ENDIF
059400020204     C                   ENDIF
059500020204     C*
059600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
059700020204     C                   Z-ADD     vabCAS        wrkDec
059800020204     C                   IF        wrkDec > *zeros
059900020204     C                   IF        vabVCA = 'ITL'
060000020204     C                   EVAL      vabCAS = *zeros
060100020204     C                   ENDIF
060200020204     C                   ENDIF
060300020204     C*
060400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060500020204     C                   Z-ADD     vabVMD        wrkDec
060600020204     C                   IF        wrkDec > *zeros
060700020204     C                   IF        vabVAD = 'ITL'
060800020204     C                   EVAL      vabVMD = *zeros
060900020204     C                   ENDIF
061000020204     C                   ENDIF
061100020204     C*
061200020204     C                   ENDSR
061300020204     C***
061400020204
061500010330
061600010330
061700010330     C*----------------------------------------------------*
061800010330     C*  CONTROLLO NUMERICITA' CAMPI
061900010330     C*----------------------------------------------------*
062000010330     C     CHKNUM        BEGSR
062100010330     C*
062200010606     C                   IF        PiDecChr = *blanks
062300030606     C                   EVAL      PiDecChr = ','
062400010606     C                   ENDIF
062500010606     C*
062600010606     C                   CALL(e)   'ISNUMERIC'
062700010330     C                   PARM                    PiStr            30
062800010606     C                   PARM                    PiDecChr          1
062900010330     C                   PARM      *ZEROS        PiVal            30 9
063000010330     C                   PARM      '0'           PiInt             1
063100010330     C                   PARM      '0'           PiNum             1
063200010330     C                   IF        %error
063300010606     C                   EVAL      PiNum=*off
063400010330     C                   ENDIF
063500010330     C*
063600010330     C                   ENDSR
063700010330     C***
063800010330
063900010601
064000010601
064100010601
064200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
064300010601     C     repfil        BEGSR
064400010601     C*
064500010601     C                   if        invfil = *zeros and
064600010601     C                             depfil > *zeros and
064700010629     C                             (vinflg = *blanks or
064800010629     C                              vinflg = *zeros)
064900010601     C
065000010601     C                   eval      invfil = depfil
065100010601     C                   endif
065200010601     C*
065300010601     C                   if        depfil <> invfil and
065400010601     C                             invfil > *zeros
065500010601     C                   eval      flgMulti = '1'
065600010601     C                   if        vinflg = *blanks
065700010601     C                   add       1             cntNonEl
065800010601     C                   endif
065900010601     C                   endif
066000010601     C*
066100010601     C                   if        vinflg = '2'
066200010601     C                   eval      flgStato = '2'
066300010601     C                   endif
066400010601     C*
066500010601     C                   ENDSR
066600010601     C***
066700040628
066800040628
066900040628
067000040628      /TITLE Invio dei dati al punto operativo.
067100040628     C     invio         BEGSR
067200040628     C*
067300040628     C* 1° invio FIVAT
067400040628     C                   reset                   dscmz
067500040628     C                   move      vlrpoi        cmzdst
067600040628     C                   eval      cmzfld = 'FIVATWWR'
067700040628     C                   eval      cmzmbd = vlrhdl
067800040628     C                   eval      %subst(cmzmbd:1:1) = 'M'
067900040628     C***                if        prmfir = *blanks
068000040628     C                   eval      cmzfla = 'FIVAT00F'
068100040628     C                   eval      cmzmba = 'FIVAT00F'
068200040628     C***                else
068300040628     C***                eval      cmzfla = prmfir
068400040628     C***                eval      cmzmba = prmfir
068500040628     C***                endif
068600040628     C                   eval      cmznrr = *zeros
068700040628     C                   move      §ctrokvt      cmznrr
068800040628     C                   eval      cmzlba = vlrfl1
068900040628     C                   call(e)   'TIS711C'
069000040628     C                   parm                    dscmz
069100040628     C                   parm      *blanks       esito
069200040628     C                   if        %error
069300040628     C                             or cmzerr = '1'
069400040628     C                             or esito  = '1'
069500040628     C                   eval      wrkesito = '3'
069600040628     C                   else
069700040628     C*
069800040628     C* 2° invio FIVAB
069900040628     C                   reset                   dscmz
070000040628     C                   move      vlrpoi        cmzdst
070100040628     C                   eval      cmzfld = vlrfou
070200040628     C                   eval      cmzmbd = vlrhdl
070300040628     C                   eval      %subst(cmzmbd:1:1) = 'M'
070400040628     C***                if        prmfir = *blanks
070500040628     C                   eval      cmzfla = 'FIVAB00F'
070600040628     C                   eval      cmzmba = 'FIVAB00F'
070700040628     C***                else
070800040628     C***                eval      cmzfla = prmfir
070900040628     C***                eval      cmzmba = prmfir
071000040628     C***                endif
071100040628     C                   eval      cmznrr = *zeros
071200040628     C                   move      §ctrokvb      cmznrr
071300040628     C                   eval      cmzlba = vlrfl1
071400040628     C                   call(e)   'TIS711C'
071500040628     C                   parm                    dscmz
071600040628     C                   parm      *blanks       esito
071700040628     C                   if        %error
071800040628     C                             or cmzerr = '1'
071900040628     C                             or esito  = '1'
072000040628     C                   eval      wrkesito = '3'
072100040628     C                   endif
072200040628     C                   endif
072300040628     C*
072400040628     C                   ENDSR
072500040628     C***
072600010601
072700010601
072800010601
072900010601      /TITLE Invio dei dati al punto operativo.
073000010601     C     opeini        BEGSR
073100010601     C*
073200010601     C* Inizializzo flag e contatori operativi
073300010601     C                   movel     '0'           flgGiro           1
073400010601     C                   movel     '0'           flgMulti          1
073500010601     C                   movel     '1'           flgStato          1
073600010615     C                   movel     '0'           flgOk             1
073700010601     C                   z-add     *zeros        cntNonEl         10 0
073800010601     C                   z-add     *zeros        depfil            3 0
073900010601     C                   z-add     *zeros        invfil            3 0
074000010601     C*
074100010601     C                   ENDSR
074200010601     C***
074300010601
074400010601
074500010601
074600010330
074700010330
074800000613     C     *inzsr        BEGSR
074900990910     C*
075000990910     C     *entry        plist
075100990920     C                   parm                    tivlrds
075200990921     C                   parm      wrkesito      esito
075300000724     C                   parm                    prmlit
075400000710     C                   parm                    prmfir
075500010330     C*
075600010330     C* CALCOLA LA DATA CORRENTE
075700010330     C                   time                    wn14             14 0
075800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075900010330     C                   z-add     wn8           g08dat
076000010330     C                   z-add     *zeros        g08inv
076100010330     C                   movel     '0'           g08err
076200010330     C                   call      'XSRDA8'
076300010330     C                   parm                    wlbda8
076400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076500000613     C*
076600000613     C                   ENDSR
076700000613     C***
076800990908
076900030606     Otitv57p   E            riepilogo         2
077000990915     O                                              'Upload via Internet'
077100990915     O                                           +1 'Traduzione TIVIN00R -'
077200021025     O                                           +1 'FIVABWWR'
077300990915     O                                           +1 'Testate bolle'
077400990915     O          E            riepilogo   2
077500990915     O                       wrkdata
077600990915     O                       wrkora              +1
077700990915     O                       procname            +1
077800990915     O          E            riepilogo   2
077900990915     O                                              'Cliente..................:'
078000010605     O                       VABCCM        z     +1
078100990915     O          E            riepilogo   2
078200990920     O                                              'Riferimento Strategi.....:'
078300990920     O                       vlrhdl              +1
078400990915     O          E            riepilogo   2
078500990915     O                                              'Giusti...................:'
078600040628     O                       §CTROKVB      2   +  1
078700990915     O          E            riepilogo   2
078800990915     O                                              'Sbagliati e corretti.....:'
078900971022     O                       §CTRMO        2   +  1
079000990915     O          E            riepilogo   2
079100990915     O                                              'Sbagliati e scartati.....:'
079200971022     O                       §CTRNO        2   +  1
079300000613
079400030606     Otitv57ps  E            testdett          2
079500000613     O                                              'Upload via Internet'
079600000613     O                                           +1 'Traduzione TIVIN00R -'
079700021025     O                                           +1 'FIVABWWR'
079800000613     O                                           +1 'Testate bolle'
079900000616     O          E            testdett    3
080000000613     O                                           +2 'N° rec'
080100000613     O                                           +3 'Anteprima contenuto'
080200000616     O          E            rigadett    2
080300000613     O                       rrnum               +2
080400000621     O                       recko               +3
080500000616     O          E            findett     2
080600000613     O                       wrkdata
080700000613     O                       wrkora              +1
080800000613     O                       procname            +1
080900000616     O          E            findett     2
081000000613     O                                              'Cliente..................:'
081100010605     O                       VABCCM        z     +1
081200000616     O          E            findett     2
081300000613     O                                              'Riferimento Strategi.....:'
081400000613     O                       vlrhdl              +1
081500000616     O          E            findett     2
081600000613     O                                              'Giusti...................:'
081700040628     O                       §CTROKVB      2   +  1
081800000616     O          E            findett     2
081900000613     O                                              'Sbagliati e corretti.....:'
082000000613     O                       §CTRMO        2   +  1
082100000616     O          E            findett     2
082200000613     O                                              'Sbagliati e scartati.....:'
082300000613     O                       §CTRNO        2   +  1
082400000613** CMD - COMANDI CL
082500030606OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV57P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
082600030606OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV57PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
082700030606DLTOVR FILE(TITV57P TITV57PS) LVL(*)
082800010606
082900000613
