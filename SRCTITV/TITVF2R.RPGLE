000100041019      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR. (x RICOH Cod. 0431960)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600041019     FFIVABWWR  O    E             DISK    usropn
000700041019     FFIVATWWR  O    E             DISK    usropn
000800041018     Ftitvf2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100041018     Ftitvf2ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D dscmz         e ds                  inz
002300010330     D psds           sds
002400010330     D  procname         *PROC
002500010330     D tivlrds       e ds                  extname(tivlr00f)
002600010330     D esito           s              1
002700010330     D prmlit          s             10
002800010330     D prmfir          s             10
002900010330     D wrkesito        s                   like(esito)
003000010330     D wrkdata         s               d
003100010330     D wrkora          s               t
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300010330     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500041018     D parccm          s              8    INZ(*blanks)
003600041018     D parmbr          s             10    INZ(*blanks)
003700041018     D paropz          s              1    INZ(*blanks)
003800041018     D chkcall         s              1    INZ(*blanks)
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
005800041018     C                   exsr      rwvabvat
005900010601     C*
006000010601     C                   seton                                        lr
006100010601
006200010601
006300010601
006400010601
006500010601     C*--------------------------------------------------------
006600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006700010601     C*--------------------------------------------------------
006800010601     C     PREELA        BEGSR
006900010601     C*
007000010601     C* SE OCCORRE SPEDIRE IN FILIALE
007100010601     C                   if        invfil <> *zeros and
007200010601     C                             flgGiro = '0'
007300010601     C*
007400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007500010601     C                   eval      flgGiro = '1'
007600000724     C*
007700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900010601     C     invfil        chain     azorg01l
008000000724     C                   if        %found
008100000724     C                   movel(p)  CMD(1)        depcmd
008200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008300000724     C*
008400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000724     C                   PARM                    depcmd
008800000724     C                   PARM                    LENGH
008900000724     C*
009000000724     C                   endif
009100000616     C*
009200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000616     C                   movel(p)  CMD(2)        depcmd
009400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009500000616     C*
009600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009700011113     C                   Z-ADD     150           LENGH            15 5
009800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009900000616     C                   PARM                    depcmd
010000000616     C                   PARM                    LENGH
010100000613     C*
010200041018     C                   if        not %open(titvf2ps)
010300041018     C                   open      titvf2ps
010400000616     C                   except    testdett
010500000613     C                   endif
010600010601     C*
010700010601     C                   endif
010800010601     C*
010900010601     C                   ENDSR
011000010601     C***
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011600010601     C*--------------------------------------------------------
011700010601     C     ENDELA        BEGSR
011800990915     C*
011900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000050117     C*
012100050117     C* Effettuo la chiamata al CLLE preposto
012200050117     C                   call(e)   'TITVVTC'
012300050117     C                   parm                    parccm
012400050117     C                   parm                    parmbr
012500050117     C                   parm      '2'           paropz
012600000613     C*
012700041018     C                   if        %open(titvf2ps)
012800000616     C                   except    findett
012900041018     C                   close     titvf2ps
013000000613     C                   endif
013100000616     C*
013200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    CMD(3)
013600000616     C                   PARM                    LENGH
013700000616     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200000613
014300000613     C*--------------------------------------------------------
014400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014500000613     C*--------------------------------------------------------
014600000621     C     STPR          BEGSR
014700000613     C*
014800041018     C                   if        not %open(titvf2p)
014900041018     C                   open      titvf2p
015000990915     C                   endif
015100990915     C*
015200990915     C                   except    riepilogo
015300990915     C*
015400041018     C                   if        %open(titvf2p)
015500041018     C                   close     titvf2p
015600990914     C                   endif
015700000613     C*
015800000613     C                   ENDSR
015900000613     C***
016000000613
016100010601
016200010601
016300010601
016400010330     C*--------------------------------------------------------
016500041019     C* RWVABVAT - LEGGE TIVIN00R E SCRIVE FIVABWWF/FIVATWWF  *
016600010330     C*--------------------------------------------------------
016700041018     C     RWVABVAT      BEGSR
016800010330     C*
016900010330     C                   if        not %open(tivin00r)
017000010330     C                   open      tivin00r
017100010330     C                   endif
017200041019     C                   if        not %open(fivabwwr)
017300041019     C                   open      fivabwwr
017400010330     C                   endif
017500041018     C*
017600041019     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017700041018     C                   exsr      prevat
017800041018     C*
017900041018     C                   if        chkcall = '0'
018000041018     C*
018100041019     C                   if        not %open(fivatwwr)
018200041019     C                   open      fivatwwr
018300041018     C                   endif
018400010330     C*
018500041018     C                   clear                   §CTROKVB          7 0
018600041018     C                   clear                   §CTROKVT          7 0
018700041018     C                   clear                   §CTRMO            7 0
018800041018     C                   clear                   §CTRNO            7 0
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
021400041019     C                   exsr      impvabvat
021500041019     C*
021600041019     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021700041019     C                   eval      depfil = VABLNP
021800041019     C                   exsr      repfil
021900041019     C                   if        depfil = invfil
022000041019     C                   if        vlrpoi = 999
022100041019     C                   MOVE(P)   invfil        VABFGS
022200041019     C                   else
022300041019     C                   MOVE(P)   vlrpoi        VABFGS
022400041019     C                   endif
022500041019     C*
022600041019     C                   exsr      PREELA
022700041019     C*
022800041019     C* X ogni testata imposto e scrivo anche i relativi record del VAT
022900041019     C  N31              EXSR      WRIVAT
023000041019     C*
023100041019     C* Ebbene...
023200041019     C*
023300041019     C  N31              ADD       1             §CTROKVB
023400041019     C   32              ADD       1             §CTRMO
023500041019     C   31              ADD       1             §CTRNO
023600041019     C*
023700041019     C  N31              WRITE     FIVAB000
023800041019     C*
023900041019     C                   if        *in31 = *off and
024000041019     C                             *in32 = *off
024100041019     C                   eval      vinflg = '1'
024200041019     C                   else
024300041019     C                   eval      recko = vindta
024400041019     C                   except    rigadett
024500041019     C                   eval      vinflg = '2'
024600041019     C                   endif
024700041019     C                   endif
024800041019     C*
024900010601     C*
025000010604     C                   endif
025100010604     C*
025200010330     C                   else
025300010330     C                   eval      vinflg = '1'
025400010330     C                   endif
025500010601     C*
025600010601     C  N70              update    tivin000
025700010330     C*
025800010330     C  N70              ENDdo
025900041018     C*
026000041018     C                   endif                                                  (endif - chkcall)
026100010601     C*
026200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026300010601     C                   if        cntNonEl = *zeros or
026400010601     C                             flgMulti = '0'
026500010330     C* Se non ci sono record con errori ...
026600010601     C                   if        §ctrno = 0 and
026700010604     C                             §ctrmo = 0 and
026800010601     C                             flgStato <> '2'
026900010330     C* ... restituisco esito OK.
027000010330     C                   eval      wrkesito = '0'
027100010330     C                   else
027200041018     C                   if        §ctrokvb > 0
027300010330     C                   eval      wrkesito = '1'
027400010330     C                   else
027500010615     C                   if        flgOk = '0'
027600010615     C                   eval      wrkesito = '2'
027700010615     C                   else
027800010615     C                   eval      wrkesito = '6'
027900010615     C                   endif
028000010330     C                   endif
028100010330     C                   endif
028200010601     C                   else
028300010601     C                   eval      wrkesito = '9'
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        %open(tivin00r)
028700010330     C                   close     tivin00r
028800010330     C                   endif
028900041019     C                   if        %open(fivabwwr)
029000041019     C                   close     fivabwwr
029100010330     C                   endif
029200041019     C                   if        %open(fivatwwr)
029300041019     C                   close     fivatwwr
029400041018     C                   endif
029500010601     C*
029600010601     C                   if        vlrpoi <> 999
029700010601     C                   eval      invfil = vlrpoi
029800010601     C                   endif
029900010330     C*
030000041018     C                   if        §ctrokvb > 0
030100010601     C                             and invfil > *zeros
030200010330     C                   exsr      invio
030300010330     C                   endif
030400010601     C*
030500010618     C                   if        flgGiro = '1'
030600010601     C                   exsr      endela
030700010618     C                   endif
030800010330     C*
030900010330     C                   ENDSR
031000010330     C***
031100010330
031200010601
031300010601
031400010601
031500010601
031600010330     C*----------------------------------------------------*
031700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031800010330     C*----------------------------------------------------*
031900040607     C     INZVAR        BEGSR
032000010330     C*
032100020204     C                   Z-ADD     *zeros        Num5_0            5 0
032200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032300020322     C                   MOVEL     '0'           FlgCAS            1
032400010330     C*
032500010330     C                   ENDSR
032600010330     C*----------------------------------------------------*
032700040607     C*  IMPOSTAZIONE CAMPI COSTANTI
032800010330     C*----------------------------------------------------*
032900010330     C     DEFCAM        BEGSR
033000010330     C*
033100020204     C* Inizializzo il buffer del record da scrivere
033200041019     C                   CLEAR                   FIVAB000
033300041019     C                   CLEAR                   FIVAT000
033400020204     C* Imposto i valori di default...
033500041018     C                   EVAL      VABCCM = 0431960
033600020403     C                   EVAL      VABCTM = '7Q'
033700041018     C                   EVAL      VABLNP = 043
033800020322     C                   EVAL      VABCTR = 000
033900041018     C                   EVAL      VABCBO = '1'
034000020204     C* ... e poi verifico se sono stati passati come parametri
034100020204     C                   IF        vlrppt > *blanks
034200040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
034300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034400020204     C                   EXSR      CHKNUM
034500020204     C                   IF        PiInt=*on
034600020204     C                   Z-ADD     PiVal         VABCCM
034700020204     C                   ENDIF
034800040607     C                   ENDIF
034900040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
035000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035100020204     C                   EXSR      CHKNUM
035200020204     C                   IF        PiInt=*on
035300020204     C                   Z-ADD     PiVal         VABLNP
035400020204     C                   ENDIF
035500040607     C                   ENDIF
035600040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
035700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035800020204     C                   EXSR      CHKNUM
035900020204     C                   IF        PiInt=*on
036000020204     C                   Z-ADD     PiVal         VABCTR
036100020204     C                   ENDIF
036200040607     C                   ENDIF
036300020204     C                   ENDIF
036400020204     C*
036500010330     C                   ENDSR
036600010607     C*----------------------------------------------------*
036700041019     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB/FIVAT)
036800010607     C*----------------------------------------------------*
036900041018     C     IMPVABVAT     BEGSR
037000010607     C*
037100010607     C                   SETOFF                                       3132
037200010607     C*
037300010607     C* Reperimento campi ALFA
037400010607     C*
037500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
037600041018     C                   EVAL      VABRSD=%trim(%subst(vindta:182:30))
037700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
037900020117     C* ==
038000041018     C*
038100060210     C                   EVAL      VABRD2=%trim(%subst(vindta:344:59))
038200041018     C                   EVAL      VABIND=%trim(%subst(vindta:212:30))
038300041018     C                   EVAL      VABLOD=%trim(%subst(vindta:242:30))
038400041018     C                   EVAL      VABPRD=%trim(%subst(vindta:272:2))
038500041018     C                   EVAL      VABRMA=%trim(%subst(vindta:27:9))
038600041018     C                   EVAL      VABTSP=%trim(%subst(vindta:2:1))
038700041018     C                   EVAL      VABNAS=%trim(%subst(vindta:282:15))
038800070308     C*                  EVAL      VABNOT=%trim(%subst(vindta:409:41))
038900070308     C*                  EVAL      VABNT2=%trim(%subst(vindta:514:40))
039000070308     C                   MOVEL     *blanks       wNOTE            70
039100070308     C                   EVAL      wNOTE=%trim(%subst(vindta:409:19))+
039200070308     C                                   %trim(%subst(vindta:465:20))+
039300070308     C                                   %trim(%subst(vindta:485:13))+
039400070308     C                                   %trim(%subst(vindta:514:18))
039500070308     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
039600070308     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
039700070308     C*
039800070308     C                   IF        %trim(%subst(vindta:510:3)) <> *blanks AND
039900070308     C                             %trim(%subst(vindta:510:3)) <> '0'
040000070308     C                   EVAL      VABTC1 = 'P'
040100070308     C                   ENDIF
040200020329     C*
040300020329     C                   MOVEL     DATCOR        VABAAS
040400020329     C                   MOVE      DATCOR        VABMGS
040500010607     C*
040600010607     C* Reperimento campi NUMERICI
040700041019     C* NSP/RMN
040800041018     C                   EVAL      PiStr=%trim(%subst(vindta:9:10))
040900020304     C                   EXSR      CHKNUM
041000020304     C                   IF        PiInt=*on
041100041019     C                   Z-ADD     PiVal         VABNSP
041200040607     C                   Z-ADD     PiVal         VABRMN
041300020304     C                   ELSE
041400041019     C                   SETON                                        31
041500041019     C                   Z-ADD     *zeros        VABNSP
041600041019     C                   Z-ADD     1             VABRMN
041700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
041800041019     C                             + ' ' + 'VABNSP VABRMN'
041900020304     C                   ENDIF
042000040607     C* CAD
042100041018     C                   EVAL      PiStr=%trim(%subst(vindta:274:5))
042200010607     C                   EXSR      CHKNUM
042300010607     C                   IF        PiInt=*on
042400020204     C                   Z-ADD     PiVal         Num5_0
042500020204     C                   MOVEL(p)  Num5_0        VABCAD
042600010607     C                   ELSE
042700010607     C                   SETON                                        32
042800020204     C                   EVAL      VABCAD = *zeros
042900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043000020204     C                             + ' ' + 'VABCAD'
043100010607     C                   ENDIF
043200040607     C* NCL
043300041018     C                   EVAL      PiStr=%trim(%subst(vindta:309:5))
043400010607     C                   EXSR      CHKNUM
043500010607     C                   IF        PiInt=*on
043600010607     C                   Z-ADD     PiVal         VABNCL
043700010607     C                   ELSE
043800010607     C                   SETON                                        32
043900010607     C                   Z-ADD     *zeros        VABNCL
044000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044100010607     C                             + ' ' + 'VABNCL'
044200010607     C                   ENDIF
044300040607     C* PKB
044400041018     C                   EVAL      PiStr=%trim(%subst(vindta:302:7))
044500010607     C                   EXSR      CHKNUM
044600010607     C                   IF        PiNum=*on
044700041025     C                   EVAL      VABPKB = PiVal/100                           * gestisco 2 decim.
044800010607     C                   ELSE
044900010607     C                   SETON                                        32
045000010607     C                   Z-ADD     *zeros        VABPKB
045100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045200010607     C                             + ' ' + 'VABPKB'
045300010607     C                   ENDIF
045400020322     C*
045500020322     C* Considerazioni finali su CBO/CAS
045600020322     C                   IF        FlgCAS = '1'
045700020322     C                   EVAL      VABCBO = '4'
045800020322     C                   ELSE
045900020322     C                   EVAL      VABCBO = '1'
046000020322     C                   ENDIF
046100020204     C*
046200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046300020204     C                   EXSR      CHKIMPDIV
046400041018     C*
046500010607     C                   ENDSR
046600010607     C*----------------------------------------------------*
046700041018
046800041018
046900041018
047000041018
047100041018     C*----------------------------------------------------*
047200041019     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047300041018     C*----------------------------------------------------*
047400041018     C     PREVAT        BEGSR
047500041018     C*
047600041019     C* Compongo il nome del membro da dare al FIVATWWR
047700041018     C                   eval      parmbr = vlrhdl
047800041018     C                   movel     'M'           parmbr
047900041018     C                   eval      parccm = vlrksc
048000041018     C                   eval      paropz = '1'
048100041018     C* Effettuo la chiamata al CLLE preposto
048200041019     C                   call(e)   'TITVVTC'
048300041018     C                   parm                    parccm
048400041018     C                   parm                    parmbr
048500041018     C                   parm                    paropz
048600041018     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048700041018     C                   if        %error
048800041018     C                   movel     '1'           chkcall
048900041018     C                   else
049000041018     C                   movel     '0'           chkcall
049100041018     C                   endif
049200041018     C*
049300041018     C                   ENDSR
049400041018
049500041018
049600041018
049700041018
049800041018     C*----------------------------------------------------*
049900041018     C*  SCARICAMENTO BUFFER RECORDS VAT
050000041018     C*----------------------------------------------------*
050100041018     C     WRIVAT        BEGSR
050200041018     C*
050300041018     C* Scrivo i tipi record del VAT relativamente a referente consegna e telefono referente
050400041018     C                   eval      VATFGS = VABFGS
050500041018     C                   eval      VATCCM = VABCCM
050600041018     C                   eval      VATLNP = VABLNP
050700041018     C                   eval      VATAAS = VABAAS
050800041018     C                   eval      VATNSP = VABNSP
050900041018     C* Record referente consegna
051000041018     C                   eval      VATTRC = 'A'
051100041018     C                   eval      VATNOT = %trim(%subst(vindta:465:20))
051200041018     C                   if        VATNOT <> *blanks
051300041019     C                   write     fivat000
051400041018     C                   add       1             §CTROKVT
051500041018     C                   endif
051600041018     C* Record telefono referente
051700041018     C                   eval      VATTRC = 'B'
051800041018     C                   eval      VATNOT = %trim(%subst(vindta:485:25))
051900041018     C                   if        VATNOT <> *blanks
052000041019     C                   write     fivat000
052100041018     C                   add       1             §CTROKVT
052200041018     C                   endif
052300041018     C*
052400041018     C                   ENDSR
052500041018
052600041018
052700010601
052800020204
052900020204     C*----------------------------------------------------*
053000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053100020204     C*----------------------------------------------------*
053200020204     C     CHKIMPDIV     BEGSR
053300020204     C*
053400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053500020204     C                   Z-ADD     *zeros        wrkDec            9 9
053600020204     C*
053700020204     C* Come prima cosa effettuo considerazioni sulla divisa
053800020204     C                   IF        vabIAS > *zeros
053900020204     C                   IF        vabVAS <> 'EUR'
054000020204     C                   EVAL      vabVAS =  'ITL'
054100020204     C                   ENDIF
054200020204     C                   ENDIF
054300020204     C*
054400020204     C                   IF        vabCAS > *zeros
054500020204     C                   IF        vabVCA <> 'EUR'
054600020204     C                   EVAL      vabVCA =  'ITL'
054700020204     C                   ENDIF
054800020204     C                   ENDIF
054900020204     C*
055000020204     C                   IF        vabVMD > *zeros
055100020321     C                   IF        vabVAD <> 'EUR'
055200020204     C                   EVAL      vabVAD =  'ITL'
055300020204     C                   ENDIF
055400020204     C                   ENDIF
055500020204     C*
055600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055700020204     C                   Z-ADD     vabIAS        wrkDec
055800020204     C                   IF        wrkDec > *zeros
055900020204     C                   IF        vabVAS = 'ITL'
056000020204     C                   EVAL      vabIAS = *zeros
056100020204     C                   ENDIF
056200020204     C                   ENDIF
056300020204     C*
056400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
056500020204     C                   Z-ADD     vabCAS        wrkDec
056600020204     C                   IF        wrkDec > *zeros
056700020204     C                   IF        vabVCA = 'ITL'
056800020204     C                   EVAL      vabCAS = *zeros
056900020204     C                   ENDIF
057000020204     C                   ENDIF
057100020204     C*
057200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057300020204     C                   Z-ADD     vabVMD        wrkDec
057400020204     C                   IF        wrkDec > *zeros
057500020204     C                   IF        vabVAD = 'ITL'
057600020204     C                   EVAL      vabVMD = *zeros
057700020204     C                   ENDIF
057800020204     C                   ENDIF
057900020204     C*
058000020204     C                   ENDSR
058100020204     C***
058200020204
058300010330
058400010330
058500010330     C*----------------------------------------------------*
058600010330     C*  CONTROLLO NUMERICITA' CAMPI
058700010330     C*----------------------------------------------------*
058800010330     C     CHKNUM        BEGSR
058900010330     C*
059000010606     C                   IF        PiDecChr = *blanks
059100010606     C                   EVAL      PiDecChr = ','
059200010606     C                   ENDIF
059300010606     C*
059400010606     C                   CALL(e)   'ISNUMERIC'
059500010330     C                   PARM                    PiStr            30
059600010606     C                   PARM                    PiDecChr          1
059700010330     C                   PARM      *ZEROS        PiVal            30 9
059800010330     C                   PARM      '0'           PiInt             1
059900010330     C                   PARM      '0'           PiNum             1
060000010330     C                   IF        %error
060100010606     C                   EVAL      PiNum=*off
060200010330     C                   ENDIF
060300010330     C*
060400010330     C                   ENDSR
060500010330     C***
060600010330
060700010601
060800010601
060900010601
061000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
061100010601     C     repfil        BEGSR
061200010601     C*
061300010601     C                   if        invfil = *zeros and
061400010601     C                             depfil > *zeros and
061500010629     C                             (vinflg = *blanks or
061600010629     C                              vinflg = *zeros)
061700010601     C
061800010601     C                   eval      invfil = depfil
061900010601     C                   endif
062000010601     C*
062100010601     C                   if        depfil <> invfil and
062200010601     C                             invfil > *zeros
062300010601     C                   eval      flgMulti = '1'
062400010601     C                   if        vinflg = *blanks
062500010601     C                   add       1             cntNonEl
062600010601     C                   endif
062700010601     C                   endif
062800010601     C*
062900010601     C                   if        vinflg = '2'
063000010601     C                   eval      flgStato = '2'
063100010601     C                   endif
063200010601     C*
063300010601     C                   ENDSR
063400010601     C***
063500010601
063600010601
063700010601
063800010330
063900010330
064000010330
064100990920      /TITLE Invio dei dati al punto operativo.
064200000613     C     invio         BEGSR
064300041018     C*
064400041019     C* 1° invio FIVAT
064500041018     C                   reset                   dscmz
064600041018     C                   move      vlrpoi        cmzdst
064700041019     C                   eval      cmzfld = 'FIVATWWR'
064800041018     C                   eval      cmzmbd = vlrhdl
064900041018     C                   eval      %subst(cmzmbd:1:1) = 'M'
065000041018     C***                if        prmfir = *blanks
065100041019     C                   eval      cmzfla = 'FIVAT00F'
065200041019     C                   eval      cmzmba = 'FIVAT00F'
065300041018     C***                else
065400041018     C***                eval      cmzfla = prmfir
065500041018     C***                eval      cmzmba = prmfir
065600041018     C***                endif
065700041018     C                   eval      cmznrr = *zeros
065800041018     C                   move      §ctrokvt      cmznrr
065900041018     C                   eval      cmzlba = vlrfl1
066000041018     C                   call(e)   'TIS711C'
066100041018     C                   parm                    dscmz
066200041018     C                   parm      *blanks       esito
066300041018     C                   if        %error
066400041018     C                             or cmzerr = '1'
066500041018     C                             or esito  = '1'
066600041018     C                   eval      wrkesito = '3'
066700041018     C                   else
066800041018     C*
066900041019     C* 2° invio FIVAB
067000041018     C                   reset                   dscmz
067100041018     C                   move      vlrpoi        cmzdst
067200041018     C                   eval      cmzfld = vlrfou
067300041018     C                   eval      cmzmbd = vlrhdl
067400041018     C                   eval      %subst(cmzmbd:1:1) = 'M'
067500041018     C***                if        prmfir = *blanks
067600041019     C                   eval      cmzfla = 'FIVAB00F'
067700041019     C                   eval      cmzmba = 'FIVAB00F'
067800041018     C***                else
067900041018     C***                eval      cmzfla = prmfir
068000041018     C***                eval      cmzmba = prmfir
068100041018     C***                endif
068200041018     C                   eval      cmznrr = *zeros
068300041018     C                   move      §ctrokvb      cmznrr
068400041018     C                   eval      cmzlba = vlrfl1
068500041018     C                   call(e)   'TIS711C'
068600041018     C                   parm                    dscmz
068700041018     C                   parm      *blanks       esito
068800041018     C                   if        %error
068900041018     C                             or cmzerr = '1'
069000041018     C                             or esito  = '1'
069100041018     C                   eval      wrkesito = '3'
069200041018     C                   endif
069300041018     C                   endif
069400990920     C*
069500000613     C                   ENDSR
069600990910
069700010601
069800010601
069900010601
070000010601
070100010601      /TITLE Invio dei dati al punto operativo.
070200010601     C     opeini        BEGSR
070300010601     C*
070400010601     C* Inizializzo flag e contatori operativi
070500010601     C                   movel     '0'           flgGiro           1
070600010601     C                   movel     '0'           flgMulti          1
070700010601     C                   movel     '1'           flgStato          1
070800010615     C                   movel     '0'           flgOk             1
070900010601     C                   z-add     *zeros        cntNonEl         10 0
071000010601     C                   z-add     *zeros        depfil            3 0
071100010601     C                   z-add     *zeros        invfil            3 0
071200010601     C*
071300010601     C                   ENDSR
071400010601     C***
071500010601
071600010601
071700070411
071800070411     C     *pssr         BEGSR
071900070411     C*
072000070411     C                   if        %open(tivin00r)
072100070411     C                   close     tivin00r
072200070411     C                   endif
072300070411     C                   if        %open(fivabwwr)
072400070411     C                   close     fivabwwr
072500070411     C                   endif
072600070411     C                   if        %open(fivatwwr)
072700070411     C                   close     fivatwwr
072800070411     C                   endif
072900070411     C*
073000070411     C* Effettuo la chiamata al CLLE preposto
073100070411     C                   call(e)   'TITVVTC'
073200070411     C                   parm                    parccm
073300070411     C                   parm                    parmbr
073400070411     C                   parm      '2'           paropz
073500070411     C*
073600070411     C                   eval      wrkesito = '2'
073700070411     C*
073800070411     C                   seton                                        LR
073900070411     C*
074000070411     C                   ENDSR     '*CANCL'
074100070411     C***
074200070411
074300010601
074400010330
074500010330
074600000613     C     *inzsr        BEGSR
074700990910     C*
074800990910     C     *entry        plist
074900990920     C                   parm                    tivlrds
075000990921     C                   parm      wrkesito      esito
075100000724     C                   parm                    prmlit
075200000710     C                   parm                    prmfir
075300010330     C*
075400010330     C* CALCOLA LA DATA CORRENTE
075500010330     C                   time                    wn14             14 0
075600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075700010330     C                   z-add     wn8           g08dat
075800010330     C                   z-add     *zeros        g08inv
075900010330     C                   movel     '0'           g08err
076000010330     C                   call      'XSRDA8'
076100010330     C                   parm                    wlbda8
076200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076300000613     C*
076400000613     C                   ENDSR
076500000613     C***
076600990908
076700041018     Otitvf2p   E            riepilogo         2
076800990915     O                                              'Upload via Internet'
076900990915     O                                           +1 'Traduzione TIVIN00R -'
077000041019     O                                           +1 'FIVABWWR'
077100990915     O                                           +1 'Testate bolle'
077200990915     O          E            riepilogo   2
077300990915     O                       wrkdata
077400990915     O                       wrkora              +1
077500990915     O                       procname            +1
077600990915     O          E            riepilogo   2
077700990915     O                                              'Cliente..................:'
077800010605     O                       VABCCM        z     +1
077900990915     O          E            riepilogo   2
078000990920     O                                              'Riferimento Strategi.....:'
078100990920     O                       vlrhdl              +1
078200990915     O          E            riepilogo   2
078300990915     O                                              'Giusti...................:'
078400041018     O                       §CTROKVB      2   +  1
078500990915     O          E            riepilogo   2
078600990915     O                                              'Sbagliati e corretti.....:'
078700971022     O                       §CTRMO        2   +  1
078800990915     O          E            riepilogo   2
078900990915     O                                              'Sbagliati e scartati.....:'
079000971022     O                       §CTRNO        2   +  1
079100000613
079200041018     Otitvf2ps  E            testdett          2
079300000613     O                                              'Upload via Internet'
079400000613     O                                           +1 'Traduzione TIVIN00R -'
079500041019     O                                           +1 'FIVABWWR'
079600000613     O                                           +1 'Testate bolle'
079700000616     O          E            testdett    3
079800000613     O                                           +2 'N° rec'
079900000613     O                                           +3 'Anteprima contenuto'
080000000616     O          E            rigadett    2
080100000613     O                       rrnum               +2
080200000621     O                       recko               +3
080300000616     O          E            findett     2
080400000613     O                       wrkdata
080500000613     O                       wrkora              +1
080600000613     O                       procname            +1
080700000616     O          E            findett     2
080800000613     O                                              'Cliente..................:'
080900010605     O                       VABCCM        z     +1
081000000616     O          E            findett     2
081100000613     O                                              'Riferimento Strategi.....:'
081200000613     O                       vlrhdl              +1
081300000616     O          E            findett     2
081400000613     O                                              'Giusti...................:'
081500041018     O                       §CTROKVB      2   +  1
081600000616     O          E            findett     2
081700000613     O                                              'Sbagliati e corretti.....:'
081800000613     O                       §CTRMO        2   +  1
081900000616     O          E            findett     2
082000000613     O                                              'Sbagliati e scartati.....:'
082100000613     O                       §CTRNO        2   +  1
082200000613** CMD - COMANDI CL
082300041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
082400041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
082500041018DLTOVR FILE(TITVF2P TITVF2PS) LVL(*)
082600010606
082700000613
