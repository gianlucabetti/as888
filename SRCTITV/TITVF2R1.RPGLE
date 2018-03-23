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
003900060911     D depDest         s              3    INZ(*blanks)
004000060911     D curDest         s              3    INZ(*blanks)
004100060911     D wGiro           s              1  0 INZ(*zeros)
004200010330     D*------------------
004300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004400010330     D*------------------
004500010330     D WLBDA8          DS                  INZ
004600010330     D  G08DAT                 1      8  0
004700010330     D  G08INV                 9     16  0
004800010330     D  G08ERR                17     17
004900010330     D  G08TGI                18     22  0
005000000313     D*
005100010330
005200010330
005300010330
005400990915     C                   time                    wrkdata
005500990915     C                   time                    wrkora
005600000913     C                   reset                   rrnum
005700990921     C                   reset                   esito
005800990921     C                   reset                   wrkesito
005900010601     C*
006000010601     C                   exsr      opeini
006100041018     C                   exsr      rwvabvat
006200010601     C*
006300010601     C                   seton                                        lr
006400010601
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007000010601     C*--------------------------------------------------------
007100010601     C     PREELA        BEGSR
007200010601     C*
007300010601     C* SE OCCORRE SPEDIRE IN FILIALE
007400010601     C                   if        invfil <> *zeros and
007500010601     C                             flgGiro = '0'
007600010601     C*
007700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007800010601     C                   eval      flgGiro = '1'
007900000724     C*
008000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200010601     C     invfil        chain     azorg01l
008300000724     C                   if        %found
008400000724     C                   movel(p)  CMD(1)        depcmd
008500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008600000724     C*
008700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000724     C                   PARM                    depcmd
009100000724     C                   PARM                    LENGH
009200000724     C*
009300000724     C                   endif
009400000616     C*
009500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009600000616     C                   movel(p)  CMD(2)        depcmd
009700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009800000616     C*
009900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010000011113     C                   Z-ADD     150           LENGH            15 5
010100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010200000616     C                   PARM                    depcmd
010300000616     C                   PARM                    LENGH
010400000613     C*
010500041018     C                   if        not %open(titvf2ps)
010600041018     C                   open      titvf2ps
010700000616     C                   except    testdett
010800000613     C                   endif
010900010601     C*
011000010601     C                   endif
011100010601     C*
011200010601     C                   ENDSR
011300010601     C***
011400010601
011500010601
011600010601
011700010601     C*--------------------------------------------------------
011800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011900010601     C*--------------------------------------------------------
012000010601     C     ENDELA        BEGSR
012100990915     C*
012200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012300050117     C*
012400050117     C* Effettuo la chiamata al CLLE preposto
012500050117     C                   call(e)   'TITVVTC'
012600050117     C                   parm                    parccm
012700050117     C                   parm                    parmbr
012800050117     C                   parm      '2'           paropz
012900000613     C*
013000041018     C                   if        %open(titvf2ps)
013100000616     C                   except    findett
013200041018     C                   close     titvf2ps
013300000613     C                   endif
013400000616     C*
013500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
013800000616     C                   PARM                    CMD(3)
013900000616     C                   PARM                    LENGH
014000000616     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500000613
014600000613     C*--------------------------------------------------------
014700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014800000613     C*--------------------------------------------------------
014900000621     C     STPR          BEGSR
015000000613     C*
015100041018     C                   if        not %open(titvf2p)
015200041018     C                   open      titvf2p
015300990915     C                   endif
015400990915     C*
015500990915     C                   except    riepilogo
015600990915     C*
015700041018     C                   if        %open(titvf2p)
015800041018     C                   close     titvf2p
015900990914     C                   endif
016000000613     C*
016100000613     C                   ENDSR
016200000613     C***
016300000613
016400010601
016500010601
016600010601
016700010330     C*--------------------------------------------------------
016800041019     C* RWVABVAT - LEGGE TIVIN00R E SCRIVE FIVABWWF/FIVATWWF  *
016900010330     C*--------------------------------------------------------
017000041018     C     RWVABVAT      BEGSR
017100010330     C*
017200010330     C                   if        not %open(tivin00r)
017300010330     C                   open      tivin00r
017400010330     C                   endif
017500041019     C                   if        not %open(fivabwwr)
017600041019     C                   open      fivabwwr
017700010330     C                   endif
017800041018     C*
017900041019     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018000041018     C                   exsr      prevat
018100041018     C*
018200041018     C                   if        chkcall = '0'
018300041018     C*
018400041019     C                   if        not %open(fivatwwr)
018500041019     C                   open      fivatwwr
018600041018     C                   endif
018700010330     C*
018800041018     C                   clear                   §CTROKVB          7 0
018900041018     C                   clear                   §CTROKVT          7 0
019000041018     C                   clear                   §CTRMO            7 0
019100041018     C                   clear                   §CTRNO            7 0
019200010330     C*
019300010330     C                   DO        *HIVAL
019400010330     C*
019500010330     C                   READ      tivin00r                               70
019600010618     C*
019700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019800010618     C                   if        vinflg = '1'
019900010618     C                   eval      flgOk = '1'
020000010618     C                   endif
020100010618     C*
020200010330     C                   if        vindta > *blanks
020300010330     C                   add       1             rrnum
020400010330     C*
020500010601     C                   if        *in70 = *off and
020600010330     C                             (vinflg = *blanks
020700010330     C                              or vinflg = '0'
020800010330     C                              or vinflg = '2')
020900010330     C*
021000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021100010711     C                   if        vinflg = *blanks or vinflg = '0'
021200010711     C                   clear                   vinmsg
021300010711     C                   endif
021400010601     C*
021500060911     C* Verifico la rottura d codice x destinatario (è pilotata dal cliente ed è 1 zozzeria
021600060911     C* ma nn c'è stato verso d trovare 1 punto d'incontro con il cliente, x cui....)
021700060911     C                   eval      curDest = %subst(vindta:704:3)
021800060911     C                   if        curDest = *all'0' OR curDest <> depDest
021900060911     C                   if        wGiro > *zeros
022000060911     C                   exsr      wrifiles
022100060911     C                   endif
022200060911     C                   Z-ADD     1             wGiro
022300060911     C                   exsr      inzvar
022400060911     C                   exsr      defcam
022500060911     C*
022600060911     C* Salvo la rottura d codice del destinatario corrente
022700060911     C                   eval      depDest = curDest
022800060911     C*
022900060911     C* X stesso destinatario invece....
023000060911     C                   else
023100060911     C                   seton                                        55
023200060911     C                   endif
023300060911     C*
023400060911     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023500060911     C                   eval      depfil = VABLNP
023600060911     C                   exsr      repfil
023700060911     C                   if        depfil = invfil
023800060911     C                   if        vlrpoi = 999
023900060911     C                   MOVE(P)   invfil        VABFGS
024000060911     C                   else
024100060911     C                   MOVE(P)   vlrpoi        VABFGS
024200060911     C                   endif
024300060911     C*
024400060911     C                   exsr      PREELA
024500060911     C*
024600060911     C* A questo punto valorizzo il buffer d output
024700060911     C                   exsr      impvabvat
024800060911     C*
024900041019     C                   if        *in31 = *off and
025000041019     C                             *in32 = *off
025100041019     C                   eval      vinflg = '1'
025200041019     C                   else
025300041019     C                   eval      recko = vindta
025400041019     C                   except    rigadett
025500041019     C                   eval      vinflg = '2'
025600041019     C                   endif
025700041019     C                   endif
025800041019     C*
025900010601     C*
026000010604     C                   endif
026100010604     C*
026200010330     C                   else
026300010330     C                   eval      vinflg = '1'
026400010330     C                   endif
026500010601     C*
026600010601     C  N70              update    tivin000
026700010330     C*
026800010330     C  N70              ENDdo
026900041018     C*
027000060911     C* Scarico quanto rimasto "in canna" dal ciclo
027100060911     C                   exsr      wrifiles
027200060911     C*
027300041018     C                   endif                                                  (endif - chkcall)
027400010601     C*
027500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027600010601     C                   if        cntNonEl = *zeros or
027700010601     C                             flgMulti = '0'
027800010330     C* Se non ci sono record con errori ...
027900010601     C                   if        §ctrno = 0 and
028000010604     C                             §ctrmo = 0 and
028100010601     C                             flgStato <> '2'
028200010330     C* ... restituisco esito OK.
028300010330     C                   eval      wrkesito = '0'
028400010330     C                   else
028500041018     C                   if        §ctrokvb > 0
028600010330     C                   eval      wrkesito = '1'
028700010330     C                   else
028800010615     C                   if        flgOk = '0'
028900010615     C                   eval      wrkesito = '2'
029000010615     C                   else
029100010615     C                   eval      wrkesito = '6'
029200010615     C                   endif
029300010330     C                   endif
029400010330     C                   endif
029500010601     C                   else
029600010601     C                   eval      wrkesito = '9'
029700010601     C                   endif
029800010330     C*
029900010330     C                   if        %open(tivin00r)
030000010330     C                   close     tivin00r
030100010330     C                   endif
030200041019     C                   if        %open(fivabwwr)
030300041019     C                   close     fivabwwr
030400010330     C                   endif
030500041019     C                   if        %open(fivatwwr)
030600041019     C                   close     fivatwwr
030700041018     C                   endif
030800010601     C*
030900010601     C                   if        vlrpoi <> 999
031000010601     C                   eval      invfil = vlrpoi
031100010601     C                   endif
031200010330     C*
031300041018     C                   if        §ctrokvb > 0
031400010601     C                             and invfil > *zeros
031500010330     C                   exsr      invio
031600010330     C                   endif
031700010601     C*
031800010618     C                   if        flgGiro = '1'
031900010601     C                   exsr      endela
032000010618     C                   endif
032100010330     C*
032200010330     C                   ENDSR
032300010330     C***
032400010601
032500010601
032600060911     C*----------------------------------------------------*
032700060911     C*  SCRITTURA BUFFER FILE OUTPUT
032800060911     C*----------------------------------------------------*
032900060911     C     WRIFILES      BEGSR
033000060911     C*
033100060911     C* Ebbene...
033200060911     C*
033300060911     C  N31              ADD       1             §CTROKVB
033400060911     C   32              ADD       1             §CTRMO
033500060911     C   31              ADD       1             §CTRNO
033600060911     C*
033700060911     C  N31              WRITE     FIVAB000
033800060911     C*
033900060911     C* Dopo lo scarico del buffer spengo l'indicatore che guida l'importazione dei campi chiave
034000060911     C                   SETOFF                                       55
034100060911     C*
034200060911     C                   ENDSR
034300010330     C*----------------------------------------------------*
034400010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034500010330     C*----------------------------------------------------*
034600040607     C     INZVAR        BEGSR
034700010330     C*
034800020204     C                   Z-ADD     *zeros        Num5_0            5 0
034900020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
035000020322     C                   MOVEL     '0'           FlgCAS            1
035100010330     C*
035200010330     C                   ENDSR
035300010330     C*----------------------------------------------------*
035400040607     C*  IMPOSTAZIONE CAMPI COSTANTI
035500010330     C*----------------------------------------------------*
035600010330     C     DEFCAM        BEGSR
035700010330     C*
035800020204     C* Inizializzo il buffer del record da scrivere
035900041019     C                   CLEAR                   FIVAB000
036000041019     C                   CLEAR                   FIVAT000
036100020204     C* Imposto i valori di default...
036200041018     C                   EVAL      VABCCM = 0431960
036300020403     C                   EVAL      VABCTM = '7Q'
036400041018     C                   EVAL      VABLNP = 043
036500020322     C                   EVAL      VABCTR = 000
036600041018     C                   EVAL      VABCBO = '1'
036700020204     C* ... e poi verifico se sono stati passati come parametri
036800020204     C                   IF        vlrppt > *blanks
036900040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
037000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037100020204     C                   EXSR      CHKNUM
037200020204     C                   IF        PiInt=*on
037300020204     C                   Z-ADD     PiVal         VABCCM
037400020204     C                   ENDIF
037500040607     C                   ENDIF
037600040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
037700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037800020204     C                   EXSR      CHKNUM
037900020204     C                   IF        PiInt=*on
038000020204     C                   Z-ADD     PiVal         VABLNP
038100020204     C                   ENDIF
038200040607     C                   ENDIF
038300040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
038400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038500020204     C                   EXSR      CHKNUM
038600020204     C                   IF        PiInt=*on
038700020204     C                   Z-ADD     PiVal         VABCTR
038800020204     C                   ENDIF
038900040607     C                   ENDIF
039000020204     C                   ENDIF
039100020204     C*
039200010330     C                   ENDSR
039300010607     C*----------------------------------------------------*
039400041019     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB/FIVAT)
039500010607     C*----------------------------------------------------*
039600041018     C     IMPVABVAT     BEGSR
039700010607     C*
039800010607     C                   SETOFF                                       3132
039900010607     C*
040000010607     C* Reperimento campi ALFA
040100010607     C*
040200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
040300041018     C                   EVAL      VABRSD=%trim(%subst(vindta:182:30))
040400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
040500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
040600020117     C* ==
040700041018     C*
040800060210     C                   EVAL      VABRD2=%trim(%subst(vindta:344:59))
040900041018     C                   EVAL      VABIND=%trim(%subst(vindta:212:30))
041000041018     C                   EVAL      VABLOD=%trim(%subst(vindta:242:30))
041100041018     C                   EVAL      VABPRD=%trim(%subst(vindta:272:2))
041200041018     C                   EVAL      VABRMA=%trim(%subst(vindta:27:9))
041300041018     C                   EVAL      VABTSP=%trim(%subst(vindta:2:1))
041400041018     C                   EVAL      VABNAS=%trim(%subst(vindta:282:15))
041500070308     C*                  IF        %subst(vindta:409:41) <> *blanks
041600070308     C*                  EVAL      VABNOT=%trim(%subst(vindta:409:41))
041700070308     C*                  ELSE
041800070308     C*                  EVAL      VABNOT=%trim(%subst(vindta:465:35))
041900070308     C*                  ENDIF
042000070308     C*                  EVAL      VABNT2=%trim(%subst(vindta:514:40))
042100070308     C                   MOVEL     *blanks       wNOTE            70
042200070308     C                   EVAL      wNOTE=%trim(%subst(vindta:409:19))+
042300070308     C                                   %trim(%subst(vindta:465:20))+
042400070308     C                                   %trim(%subst(vindta:485:13))+
042500070308     C                                   %trim(%subst(vindta:514:18))
042600070308     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
042700070308     C                   EVAL      VABNT2=%trim(%subst(wNOTE:36:35))
042800070308     C*
042900070308     C                   IF        %trim(%subst(vindta:510:3)) <> *blanks AND
043000070308     C                             %trim(%subst(vindta:510:3)) <> '0'
043100070308     C                   EVAL      VABTC1 = 'P'
043200070308     C                   ENDIF
043300020329     C*
043400020329     C                   MOVEL     DATCOR        VABAAS
043500020329     C                   MOVE      DATCOR        VABMGS
043600010607     C*
043700010607     C* Reperimento campi NUMERICI
043800041019     C* NSP/RMN
043900060911     C                   IF        *IN55 = *OFF
044000041018     C                   EVAL      PiStr=%trim(%subst(vindta:9:10))
044100020304     C                   EXSR      CHKNUM
044200020304     C                   IF        PiInt=*on
044300041019     C                   Z-ADD     PiVal         VABNSP
044400040607     C                   Z-ADD     PiVal         VABRMN
044500020304     C                   ELSE
044600041019     C                   SETON                                        31
044700041019     C                   Z-ADD     *zeros        VABNSP
044800041019     C                   Z-ADD     1             VABRMN
044900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
045000041019     C                             + ' ' + 'VABNSP VABRMN'
045100020304     C                   ENDIF
045200060911     C                   ENDIF
045300040607     C* CAD
045400041018     C                   EVAL      PiStr=%trim(%subst(vindta:274:5))
045500010607     C                   EXSR      CHKNUM
045600010607     C                   IF        PiInt=*on
045700020204     C                   Z-ADD     PiVal         Num5_0
045800020204     C                   MOVEL(p)  Num5_0        VABCAD
045900010607     C                   ELSE
046000010607     C                   SETON                                        32
046100020204     C                   EVAL      VABCAD = *zeros
046200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
046300020204     C                             + ' ' + 'VABCAD'
046400010607     C                   ENDIF
046500040607     C* NCL
046600041018     C                   EVAL      PiStr=%trim(%subst(vindta:309:5))
046700010607     C                   EXSR      CHKNUM
046800010607     C                   IF        PiInt=*on
046900060911     C                   ADD       PiVal         VABNCL
047000010607     C                   ELSE
047100010607     C                   SETON                                        32
047200010607     C                   Z-ADD     *zeros        VABNCL
047300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
047400010607     C                             + ' ' + 'VABNCL'
047500010607     C                   ENDIF
047600040607     C* PKB
047700041018     C                   EVAL      PiStr=%trim(%subst(vindta:302:7))
047800010607     C                   EXSR      CHKNUM
047900010607     C                   IF        PiNum=*on
048000060911     C                   EVAL      PiVal = PiVal/100                            * gestisco 2 decim.
048100060911     C                   ADD       PiVal         VABPKB
048200010607     C                   ELSE
048300010607     C                   SETON                                        32
048400010607     C                   Z-ADD     *zeros        VABPKB
048500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
048600010607     C                             + ' ' + 'VABPKB'
048700010607     C                   ENDIF
048800020322     C*
048900020322     C* Considerazioni finali su CBO/CAS
049000020322     C                   IF        FlgCAS = '1'
049100020322     C                   EVAL      VABCBO = '4'
049200020322     C                   ELSE
049300020322     C                   EVAL      VABCBO = '1'
049400020322     C                   ENDIF
049500020204     C*
049600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049700020204     C                   EXSR      CHKIMPDIV
049800070306     C*
049900070306     C* X ogni testata imposto e scrivo anche i relativi record del VAT
050000070306     C  N31              EXSR      WRIVAT
050100041018     C*
050200010607     C                   ENDSR
050300010607     C*----------------------------------------------------*
050400041018
050500041018
050600041018
050700041018
050800041018     C*----------------------------------------------------*
050900041019     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051000041018     C*----------------------------------------------------*
051100041018     C     PREVAT        BEGSR
051200041018     C*
051300041019     C* Compongo il nome del membro da dare al FIVATWWR
051400041018     C                   eval      parmbr = vlrhdl
051500041018     C                   movel     'M'           parmbr
051600041018     C                   eval      parccm = vlrksc
051700041018     C                   eval      paropz = '1'
051800041018     C* Effettuo la chiamata al CLLE preposto
051900041019     C                   call(e)   'TITVVTC'
052000041018     C                   parm                    parccm
052100041018     C                   parm                    parmbr
052200041018     C                   parm                    paropz
052300041018     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052400041018     C                   if        %error
052500041018     C                   movel     '1'           chkcall
052600041018     C                   else
052700041018     C                   movel     '0'           chkcall
052800041018     C                   endif
052900041018     C*
053000041018     C                   ENDSR
053100041018
053200041018
053300041018
053400041018
053500041018     C*----------------------------------------------------*
053600041018     C*  SCARICAMENTO BUFFER RECORDS VAT
053700041018     C*----------------------------------------------------*
053800041018     C     WRIVAT        BEGSR
053900041018     C*
054000041018     C* Scrivo i tipi record del VAT relativamente a referente consegna e telefono referente
054100041018     C                   eval      VATFGS = VABFGS
054200041018     C                   eval      VATCCM = VABCCM
054300041018     C                   eval      VATLNP = VABLNP
054400041018     C                   eval      VATAAS = VABAAS
054500041018     C                   eval      VATNSP = VABNSP
054600041018     C* Record referente consegna
054700041018     C                   eval      VATTRC = 'A'
054800041018     C                   eval      VATNOT = %trim(%subst(vindta:465:20))
054900041018     C                   if        VATNOT <> *blanks
055000041019     C                   write     fivat000
055100041018     C                   add       1             §CTROKVT
055200041018     C                   endif
055300041018     C* Record telefono referente
055400041018     C                   eval      VATTRC = 'B'
055500041018     C                   eval      VATNOT = %trim(%subst(vindta:485:25))
055600041018     C                   if        VATNOT <> *blanks
055700041019     C                   write     fivat000
055800041018     C                   add       1             §CTROKVT
055900041018     C                   endif
056000041018     C*
056100041018     C                   ENDSR
056200041018
056300041018
056400010601
056500020204
056600020204     C*----------------------------------------------------*
056700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056800020204     C*----------------------------------------------------*
056900020204     C     CHKIMPDIV     BEGSR
057000020204     C*
057100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057200020204     C                   Z-ADD     *zeros        wrkDec            9 9
057300020204     C*
057400020204     C* Come prima cosa effettuo considerazioni sulla divisa
057500020204     C                   IF        vabIAS > *zeros
057600020204     C                   IF        vabVAS <> 'EUR'
057700020204     C                   EVAL      vabVAS =  'ITL'
057800020204     C                   ENDIF
057900020204     C                   ENDIF
058000020204     C*
058100020204     C                   IF        vabCAS > *zeros
058200020204     C                   IF        vabVCA <> 'EUR'
058300020204     C                   EVAL      vabVCA =  'ITL'
058400020204     C                   ENDIF
058500020204     C                   ENDIF
058600020204     C*
058700020204     C                   IF        vabVMD > *zeros
058800020321     C                   IF        vabVAD <> 'EUR'
058900020204     C                   EVAL      vabVAD =  'ITL'
059000020204     C                   ENDIF
059100020204     C                   ENDIF
059200020204     C*
059300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059400020204     C                   Z-ADD     vabIAS        wrkDec
059500020204     C                   IF        wrkDec > *zeros
059600020204     C                   IF        vabVAS = 'ITL'
059700020204     C                   EVAL      vabIAS = *zeros
059800020204     C                   ENDIF
059900020204     C                   ENDIF
060000020204     C*
060100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
060200020204     C                   Z-ADD     vabCAS        wrkDec
060300020204     C                   IF        wrkDec > *zeros
060400020204     C                   IF        vabVCA = 'ITL'
060500020204     C                   EVAL      vabCAS = *zeros
060600020204     C                   ENDIF
060700020204     C                   ENDIF
060800020204     C*
060900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061000020204     C                   Z-ADD     vabVMD        wrkDec
061100020204     C                   IF        wrkDec > *zeros
061200020204     C                   IF        vabVAD = 'ITL'
061300020204     C                   EVAL      vabVMD = *zeros
061400020204     C                   ENDIF
061500020204     C                   ENDIF
061600020204     C*
061700020204     C                   ENDSR
061800020204     C***
061900020204
062000010330
062100010330
062200010330     C*----------------------------------------------------*
062300010330     C*  CONTROLLO NUMERICITA' CAMPI
062400010330     C*----------------------------------------------------*
062500010330     C     CHKNUM        BEGSR
062600010330     C*
062700010606     C                   IF        PiDecChr = *blanks
062800010606     C                   EVAL      PiDecChr = ','
062900010606     C                   ENDIF
063000010606     C*
063100010606     C                   CALL(e)   'ISNUMERIC'
063200010330     C                   PARM                    PiStr            30
063300010606     C                   PARM                    PiDecChr          1
063400010330     C                   PARM      *ZEROS        PiVal            30 9
063500010330     C                   PARM      '0'           PiInt             1
063600010330     C                   PARM      '0'           PiNum             1
063700010330     C                   IF        %error
063800010606     C                   EVAL      PiNum=*off
063900010330     C                   ENDIF
064000010330     C*
064100010330     C                   ENDSR
064200010330     C***
064300010330
064400010601
064500010601
064600010601
064700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
064800010601     C     repfil        BEGSR
064900010601     C*
065000010601     C                   if        invfil = *zeros and
065100010601     C                             depfil > *zeros and
065200010629     C                             (vinflg = *blanks or
065300010629     C                              vinflg = *zeros)
065400010601     C
065500010601     C                   eval      invfil = depfil
065600010601     C                   endif
065700010601     C*
065800010601     C                   if        depfil <> invfil and
065900010601     C                             invfil > *zeros
066000010601     C                   eval      flgMulti = '1'
066100010601     C                   if        vinflg = *blanks
066200010601     C                   add       1             cntNonEl
066300010601     C                   endif
066400010601     C                   endif
066500010601     C*
066600010601     C                   if        vinflg = '2'
066700010601     C                   eval      flgStato = '2'
066800010601     C                   endif
066900010601     C*
067000010601     C                   ENDSR
067100010601     C***
067200010601
067300010601
067400010601
067500010330
067600010330
067700010330
067800990920      /TITLE Invio dei dati al punto operativo.
067900000613     C     invio         BEGSR
068000041018     C*
068100041019     C* 1° invio FIVAT
068200041018     C                   reset                   dscmz
068300041018     C                   move      vlrpoi        cmzdst
068400041019     C                   eval      cmzfld = 'FIVATWWR'
068500041018     C                   eval      cmzmbd = vlrhdl
068600041018     C                   eval      %subst(cmzmbd:1:1) = 'M'
068700041018     C***                if        prmfir = *blanks
068800041019     C                   eval      cmzfla = 'FIVAT00F'
068900041019     C                   eval      cmzmba = 'FIVAT00F'
069000041018     C***                else
069100041018     C***                eval      cmzfla = prmfir
069200041018     C***                eval      cmzmba = prmfir
069300041018     C***                endif
069400041018     C                   eval      cmznrr = *zeros
069500041018     C                   move      §ctrokvt      cmznrr
069600041018     C                   eval      cmzlba = vlrfl1
069700041018     C                   call(e)   'TIS711C'
069800041018     C                   parm                    dscmz
069900041018     C                   parm      *blanks       esito
070000041018     C                   if        %error
070100041018     C                             or cmzerr = '1'
070200041018     C                             or esito  = '1'
070300041018     C                   eval      wrkesito = '3'
070400041018     C                   else
070500041018     C*
070600041019     C* 2° invio FIVAB
070700041018     C                   reset                   dscmz
070800041018     C                   move      vlrpoi        cmzdst
070900041018     C                   eval      cmzfld = vlrfou
071000041018     C                   eval      cmzmbd = vlrhdl
071100041018     C                   eval      %subst(cmzmbd:1:1) = 'M'
071200041018     C***                if        prmfir = *blanks
071300041019     C                   eval      cmzfla = 'FIVAB00F'
071400041019     C                   eval      cmzmba = 'FIVAB00F'
071500041018     C***                else
071600041018     C***                eval      cmzfla = prmfir
071700041018     C***                eval      cmzmba = prmfir
071800041018     C***                endif
071900041018     C                   eval      cmznrr = *zeros
072000041018     C                   move      §ctrokvb      cmznrr
072100041018     C                   eval      cmzlba = vlrfl1
072200041018     C                   call(e)   'TIS711C'
072300041018     C                   parm                    dscmz
072400041018     C                   parm      *blanks       esito
072500041018     C                   if        %error
072600041018     C                             or cmzerr = '1'
072700041018     C                             or esito  = '1'
072800041018     C                   eval      wrkesito = '3'
072900041018     C                   endif
073000041018     C                   endif
073100990920     C*
073200000613     C                   ENDSR
073300990910
073400010601
073500010601
073600010601
073700010601
073800010601      /TITLE Invio dei dati al punto operativo.
073900010601     C     opeini        BEGSR
074000010601     C*
074100010601     C* Inizializzo flag e contatori operativi
074200010601     C                   movel     '0'           flgGiro           1
074300010601     C                   movel     '0'           flgMulti          1
074400010601     C                   movel     '1'           flgStato          1
074500010615     C                   movel     '0'           flgOk             1
074600010601     C                   z-add     *zeros        cntNonEl         10 0
074700010601     C                   z-add     *zeros        depfil            3 0
074800010601     C                   z-add     *zeros        invfil            3 0
074900010601     C*
075000010601     C                   ENDSR
075100010601     C***
075200010601
075300010601
075400070411
075500070411     C     *pssr         BEGSR
075600070411     C*
075700070411     C                   if        %open(tivin00r)
075800070411     C                   close     tivin00r
075900070411     C                   endif
076000070411     C                   if        %open(fivabwwr)
076100070411     C                   close     fivabwwr
076200070411     C                   endif
076300070411     C                   if        %open(fivatwwr)
076400070411     C                   close     fivatwwr
076500070411     C                   endif
076600070411     C*
076700070411     C* Effettuo la chiamata al CLLE preposto
076800070411     C                   call(e)   'TITVVTC'
076900070411     C                   parm                    parccm
077000070411     C                   parm                    parmbr
077100070411     C                   parm      '2'           paropz
077200070411     C*
077300070411     C                   eval      wrkesito = '2'
077400070411     C*
077500070411     C                   seton                                        LR
077600070411     C*
077700070411     C                   ENDSR     '*CANCL'
077800070411     C***
077900070411
078000010601
078100010330
078200010330
078300000613     C     *inzsr        BEGSR
078400990910     C*
078500990910     C     *entry        plist
078600990920     C                   parm                    tivlrds
078700990921     C                   parm      wrkesito      esito
078800000724     C                   parm                    prmlit
078900000710     C                   parm                    prmfir
079000010330     C*
079100010330     C* CALCOLA LA DATA CORRENTE
079200010330     C                   time                    wn14             14 0
079300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
079400010330     C                   z-add     wn8           g08dat
079500010330     C                   z-add     *zeros        g08inv
079600010330     C                   movel     '0'           g08err
079700010330     C                   call      'XSRDA8'
079800010330     C                   parm                    wlbda8
079900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
080000000613     C*
080100000613     C                   ENDSR
080200000613     C***
080300990908
080400041018     Otitvf2p   E            riepilogo         2
080500990915     O                                              'Upload via Internet'
080600990915     O                                           +1 'Traduzione TIVIN00R -'
080700041019     O                                           +1 'FIVABWWR'
080800990915     O                                           +1 'Testate bolle'
080900990915     O          E            riepilogo   2
081000990915     O                       wrkdata
081100990915     O                       wrkora              +1
081200990915     O                       procname            +1
081300990915     O          E            riepilogo   2
081400990915     O                                              'Cliente..................:'
081500010605     O                       VABCCM        z     +1
081600990915     O          E            riepilogo   2
081700990920     O                                              'Riferimento Strategi.....:'
081800990920     O                       vlrhdl              +1
081900990915     O          E            riepilogo   2
082000990915     O                                              'Giusti...................:'
082100041018     O                       §CTROKVB      2   +  1
082200990915     O          E            riepilogo   2
082300990915     O                                              'Sbagliati e corretti.....:'
082400971022     O                       §CTRMO        2   +  1
082500990915     O          E            riepilogo   2
082600990915     O                                              'Sbagliati e scartati.....:'
082700971022     O                       §CTRNO        2   +  1
082800000613
082900041018     Otitvf2ps  E            testdett          2
083000000613     O                                              'Upload via Internet'
083100000613     O                                           +1 'Traduzione TIVIN00R -'
083200041019     O                                           +1 'FIVABWWR'
083300000613     O                                           +1 'Testate bolle'
083400000616     O          E            testdett    3
083500000613     O                                           +2 'N° rec'
083600000613     O                                           +3 'Anteprima contenuto'
083700000616     O          E            rigadett    2
083800000613     O                       rrnum               +2
083900000621     O                       recko               +3
084000000616     O          E            findett     2
084100000613     O                       wrkdata
084200000613     O                       wrkora              +1
084300000613     O                       procname            +1
084400000616     O          E            findett     2
084500000613     O                                              'Cliente..................:'
084600010605     O                       VABCCM        z     +1
084700000616     O          E            findett     2
084800000613     O                                              'Riferimento Strategi.....:'
084900000613     O                       vlrhdl              +1
085000000616     O          E            findett     2
085100000613     O                                              'Giusti...................:'
085200041018     O                       §CTROKVB      2   +  1
085300000616     O          E            findett     2
085400000613     O                                              'Sbagliati e corretti.....:'
085500000613     O                       §CTRMO        2   +  1
085600000616     O          E            findett     2
085700000613     O                                              'Sbagliati e scartati.....:'
085800000613     O                       §CTRNO        2   +  1
085900000613** CMD - COMANDI CL
086000041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF2P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
086100041018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVF2PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
086200041018DLTOVR FILE(TITVF2P TITVF2PS) LVL(*)
086300010606
086400000613
