000100060504      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR (x SCHENKER Cod. 2493133)
000200101105     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040723     FFIVABwwr  O    E             DISK    usropn
000700060504     FFIVATwwr  O    E             DISK    usropn
000800040928     Ftitve8p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000040928     Ftitve8ps  O    f  198        PRINTER usropn
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
002400040607     D tisi95ds      e ds
002500010330     D esito           s              1
002600010330     D prmlit          s             10
002700010330     D prmfir          s             10
002800010330     D wrkesito        s                   like(esito)
002900010330     D wrkdata         s               d
003000010330     D wrkora          s               t
003100010330     D rrnum           s              6  0 INZ(*zeros)
003200010330     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400060504     D parccm          s              8    INZ(*blanks)
003500060504     D parmbr          s             10    INZ(*blanks)
003600060504     D paropz          s              1    INZ(*blanks)
003700060504     D chkcall         s              1    INZ(*blanks)
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
004900010330
005000990915     C                   time                    wrkdata
005100990915     C                   time                    wrkora
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500010601     C*
005600010601     C                   exsr      opeini
005700010605     C                   exsr      rwvab
005800060504     C*
005900060504     C* Effettuo la chiamata al CLLE preposto
006000060504     C                   call(e)   'TITVVTC'
006100060504     C                   parm                    parccm
006200060504     C                   parm                    parmbr
006300060504     C                   parm      '2'           paropz
006400040928     C*
006500040928     C* Lancio TISI95R solo x chiusura
006600040928     C                   CLEAR                   TISI95DS
006700040928     C                   EVAL      I95TLA = 'C'
006800040928     C                   CALL      'TISI95R'
006900040928     C                   PARM                    TISI95DS
007000010601     C*
007100010601     C                   seton                                        lr
007200010601
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007800010601     C*--------------------------------------------------------
007900010601     C     PREELA        BEGSR
008000010601     C*
008100010601     C* SE OCCORRE SPEDIRE IN FILIALE
008200010601     C                   if        invfil <> *zeros and
008300010601     C                             flgGiro = '0'
008400010601     C*
008500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008600010601     C                   eval      flgGiro = '1'
008700000724     C*
008800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000010601     C     invfil        chain     azorg01l
009100000724     C                   if        %found
009200000724     C                   movel(p)  CMD(1)        depcmd
009300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009400000724     C*
009500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009600011113     C                   Z-ADD     150           LENGH            15 5
009700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800000724     C                   PARM                    depcmd
009900000724     C                   PARM                    LENGH
010000000724     C*
010100000724     C                   endif
010200000616     C*
010300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010400000616     C                   movel(p)  CMD(2)        depcmd
010500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010600000616     C*
010700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010800011113     C                   Z-ADD     150           LENGH            15 5
010900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000616     C                   PARM                    depcmd
011100000616     C                   PARM                    LENGH
011200000613     C*
011300040928     C                   if        not %open(titve8ps)
011400040928     C                   open      titve8ps
011500000616     C                   except    testdett
011600000613     C                   endif
011700010601     C*
011800010601     C                   endif
011900010601     C*
012000010601     C                   ENDSR
012100010601     C***
012200010601
012300010601
012400010601
012500010601
012600010601
012700010601
012800010601
012900010601     C*--------------------------------------------------------
013000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013100010601     C*--------------------------------------------------------
013200010601     C     ENDELA        BEGSR
013300990915     C*
013400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013500000613     C*
013600040928     C                   if        %open(titve8ps)
013700000616     C                   except    findett
013800040928     C                   close     titve8ps
013900000613     C                   endif
014000000616     C*
014100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014200011113     C                   Z-ADD     150           LENGH            15 5
014300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014400000616     C                   PARM                    CMD(3)
014500000616     C                   PARM                    LENGH
014600000616     C*
014700010601     C                   ENDSR
014800010601     C***
014900010601
015000010601
015100010601
015200010601
015300000613
015400000613     C*--------------------------------------------------------
015500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015600000613     C*--------------------------------------------------------
015700000621     C     STPR          BEGSR
015800000613     C*
015900040928     C                   if        not %open(titve8p)
016000040928     C                   open      titve8p
016100990915     C                   endif
016200990915     C*
016300990915     C                   except    riepilogo
016400990915     C*
016500040928     C                   if        %open(titve8p)
016600040928     C                   close     titve8p
016700990914     C                   endif
016800000613     C*
016900000613     C                   ENDSR
017000000613     C***
017100000613
017200010330
017300010601
017400010601
017500010601
017600010330     C*--------------------------------------------------------
017700040723     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
017800010330     C*--------------------------------------------------------
017900010605     C     RWVAB         BEGSR
018000010330     C*
018100010330     C                   if        not %open(tivin00r)
018200010330     C                   open      tivin00r
018300010330     C                   endif
018400040723     C                   if        not %open(fivabwwr)
018500040723     C                   open      fivabwwr
018600010330     C                   endif
018700060504     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018800060504     C                   exsr      prevat
018900060504     C*
019000060504     C                   if        chkcall = '0'
019100060504     C*
019200060504     C                   if        not %open(fivatwwr)
019300060504     C                   open      fivatwwr
019400060504     C                   endif
019500010330     C*
019600060504     C                   clear                   §CTROK
019700060504     C                   clear                   §CTROKVT          5 0
019800010604     C                   clear                   §CTRMO
019900010604     C                   clear                   §CTRNO
020000010330     C*
020100010330     C                   DO        *HIVAL
020200010330     C*
020300010330     C                   READ      tivin00r                               70
020400010618     C*
020500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020600010618     C                   if        vinflg = '1'
020700010618     C                   eval      flgOk = '1'
020800010618     C                   endif
020900010618     C*
021000010330     C                   if        vindta > *blanks
021100010330     C                   add       1             rrnum
021200010330     C*
021300010601     C                   if        *in70 = *off and
021400010330     C                             (vinflg = *blanks
021500010330     C                              or vinflg = '0'
021600010330     C                              or vinflg = '2')
021700010330     C*
021800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021900010711     C                   if        vinflg = *blanks or vinflg = '0'
022000010711     C                   clear                   vinmsg
022100010711     C                   endif
022200010601     C*
022300010605     C                   exsr      impvab
022400010601     C*
022500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022600010605     C                   eval      depfil = VABLNP
022700010601     C                   exsr      repfil
022800010601     C                   if        depfil = invfil
022900021025     C                   if        vlrpoi = 999
023000021025     C                   MOVE(P)   invfil        VABFGS
023100021025     C                   else
023200021025     C                   MOVE(P)   vlrpoi        VABFGS
023300021025     C                   endif
023400010601     C*
023500010601     C                   exsr      PREELA
023600010601     C*
023700010604     C* Ebbene...
023800010604     C*
023900010604     C  N31              ADD       1             §CTROK            7 0
024000010604     C   32              ADD       1             §CTRMO            7 0
024100010604     C   31              ADD       1             §CTRNO            7 0
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
026200060504     C*
026300060504     C                   endif                                                  * chkcall
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
027500010330     C                   if        §ctrok > 0
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
029200040723     C                   if        %open(fivabwwr)
029300040723     C                   close     fivabwwr
029400010330     C                   endif
029500060504     C                   if        %open(fivatwwr)
029600060504     C                   close     fivatwwr
029700060504     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300010330     C                   if        §ctrok > 0
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
031400010330
031500010601
031600010601
031700010601
031800010601
031900010330     C*----------------------------------------------------*
032000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032100010330     C*----------------------------------------------------*
032200040607     C     INZVAR        BEGSR
032300010330     C*
032400020204     C                   Z-ADD     *zeros        Num5_0            5 0
032500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032600041011     C                   MOVEL     '0'           FlgCAS            1
032700041011     C* Imposto un flag che mi indica se ho già elaborato almeno 1 record del file del cliente
032800041011     C                   MOVEL     'N'           wGiro             1
032900041012     C* Imposto un flag che mi indica se trattasi d bolla da considerare (condiziona write del VAB)
033000041012     C                   MOVEL     'S'           wWrtVAB           1
033100010330     C*
033200010330     C                   ENDSR
033300010330     C*----------------------------------------------------*
033400040607     C*  IMPOSTAZIONE CAMPI COSTANTI
033500010330     C*----------------------------------------------------*
033600010330     C     DEFCAM        BEGSR
033700010330     C*
033800020204     C* Inizializzo il buffer del record da scrivere
033900040723     C                   CLEAR                   FIVAB000
034000020204     C* Imposto i valori di default...
034100040928     C                   EVAL      VABCCM = 2493133
034200040928     C                   EVAL      VABLNP = 249
034300020322     C                   EVAL      VABCTR = 000
034400060504     C                   EVAL      VABCTM = '7Q'
034500020204     C* ... e poi verifico se sono stati passati come parametri
034600020204     C                   IF        vlrppt > *blanks
034700040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
034800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034900020204     C                   EXSR      CHKNUM
035000020204     C                   IF        PiInt=*on
035100020204     C                   Z-ADD     PiVal         VABCCM
035200020204     C                   ENDIF
035300040607     C                   ENDIF
035400040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
035500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035600020204     C                   EXSR      CHKNUM
035700020204     C                   IF        PiInt=*on
035800020204     C                   Z-ADD     PiVal         VABLNP
035900020204     C                   ENDIF
036000040607     C                   ENDIF
036100040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
036200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036300020204     C                   EXSR      CHKNUM
036400020204     C                   IF        PiInt=*on
036500020204     C                   Z-ADD     PiVal         VABCTR
036600020204     C                   ENDIF
036700040607     C                   ENDIF
036800020204     C                   ENDIF
036900020204     C*
037000010330     C                   ENDSR
037100010607     C*----------------------------------------------------*
037200040723     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
037300010607     C*----------------------------------------------------*
037400010607     C     IMPVAB        BEGSR
037500010607     C*
037600040928     C* Reperimento campi ALFA a seconda del tipo record
037700040928     C                   IF        %trim(%subst(vindta:1:3)) = 'SWS'
037800041011     C*
037900041011     C* Ad ogni nuova spedizione scarico il buffer d quella precedente
038000041012     C                   IF        wGiro = 'S' AND wWrtVAB = 'S'
038100041011     C* Considerazioni finali su CBO/CAS
038200041011     C                   IF        FlgCAS = '1'
038300041011     C                   EVAL      VABCBO = '4'
038400041011     C                   ELSE
038500041011     C                   EVAL      VABCBO = '1'
038600041011     C                   ENDIF
038700041011     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038800041011     C                   EXSR      CHKIMPDIV
038900041011     C* Scarico il buffer del FIVAB
039000041011     C  N31              WRITE     FIVAB000
039100041011     C                   ENDIF
039200041011     C*
039300040928     C                   exsr      inzvar
039400040928     C                   exsr      defcam
039500040928     C                   MOVEL     DATCOR        VABAAS
039600040928     C                   MOVE      DATCOR        VABMGS
039700040928     C                   SETOFF                                       3132
039800041011     C                   MOVEL     'S'           wGiro
039900040928     C                   ENDIF
040000040928     C*
040100041011     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20FF'
040200040928     C* RMA
040300041011     C                   EVAL      VABRMA=%trim(%subst(vindta:8:10))
040400041011     C                   ENDIF
040500040928     C*
040600041011     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20AEG'
040700041011     C* NSP/RMN
040800041012     C                   EVAL      PiStr=%trim(%subst(vindta:8:10))
040900041011     C                   EXSR      CHKNUM
041000041011     C                   IF        PiInt=*on
041100041011     C                   Z-ADD     PiVal         VABNSP
041200041011     C                   Z-ADD     PiVal         VABRMN
041300041011     C                   ELSE
041400041012     C* NSP/RMN
041500041012     C                   EVAL      PiStr=%trim(%subst(vindta:12:6))
041600041012     C                   EXSR      CHKNUM
041700041012     C                   IF        PiInt=*on
041800041012     C                   Z-ADD     PiVal         VABNSP
041900041012     C                   Z-ADD     PiVal         VABRMN
042000041012     C                   ELSE
042100041012     C                   SETON                                        32
042200041012     C                   Z-ADD     *zeros        VABNSP
042300041012     C                   Z-ADD     1             VABRMN
042400041012     C                   EVAL      vinmsg = %trimr(vinmsg)
042500041012     C                             + ' ' + 'VABNSP VABRMN'
042600041012     C                   ENDIF
042700041011     C                   ENDIF
042800040928     C                   ENDIF
042900040928     C*
043000040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40CN'
043100040928     C* RSD
043200040928     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
043300040928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043400040928     C     '@':'A'       XLATE     VABRSD        VABRSD
043500040928     C* ==
043600040928     C* RD2
043700040928     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
043800040928     C* IND
043900040928     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
044000040928     C* NT2
044100040928     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
044200040928     C* LOD
044300040928     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
044400040928     C* CAD
044500040928     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
044600040928     C                   EXSR      CHKNUM
044700040928     C                   IF        PiInt=*on
044800040928     C                   Z-ADD     PiVal         Num5_0
044900040928     C                   MOVEL(p)  Num5_0        VABCAD
045000040928     C                   ELSE
045100040928     C                   SETON                                        32
045200040928     C                   EVAL      VABCAD = *zeros
045300040928     C                   EVAL      vinmsg = %trimr(vinmsg)
045400040928     C                             + ' ' + 'VABCAD'
045500040928     C                   ENDIF
045600040928     C* Reperisco la provincia dal CAP e dalla località
045700040928     C                   IF        VABCAD <> *blanks
045800040928     C                   CLEAR                   TISI95DS
045900040928     C                   EVAL      I95TCN = '3'
046000040928     C                   Z-ADD     datcor        I95DAT
046100040928     C                   EVAL      I95CAP = VABCAD
046200040928     C                   EVAL      I95LOC = VABLOD
046300040928     C                   CALL      'TISI95R'
046400040928     C                   PARM                    TISI95DS
046500040928     C                   EVAL      VABPRD = O95PRV
046600040928     C                   ENDIF
046700040928     C                   ENDIF
046800041015     C*
046900041015     C* Se presente il tipo record 'RT40DP' by-passa il tipo record 'RT40CN'...  x cui:
047000041015     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40DP'
047100041015     C* RSD
047200041015     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
047300041015     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
047400041015     C     '@':'A'       XLATE     VABRSD        VABRSD
047500041015     C* ==
047600041015     C* RD2
047700041015     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
047800041015     C* IND
047900041015     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
048000041015     C* NT2
048100041015     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
048200041015     C* LOD
048300041015     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
048400041015     C* CAD
048500041015     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
048600041015     C                   EXSR      CHKNUM
048700041015     C                   IF        PiInt=*on
048800041015     C                   Z-ADD     PiVal         Num5_0
048900041015     C                   MOVEL(p)  Num5_0        VABCAD
049000041015     C                   ELSE
049100041015     C                   SETON                                        32
049200041015     C                   EVAL      VABCAD = *zeros
049300041015     C                   EVAL      vinmsg = %trimr(vinmsg)
049400041015     C                             + ' ' + 'VABCAD'
049500041015     C                   ENDIF
049600041015     C* Reperisco la provincia dal CAP e dalla località
049700041015     C                   IF        VABCAD <> *blanks
049800041015     C                   CLEAR                   TISI95DS
049900041015     C                   EVAL      I95TCN = '3'
050000041015     C                   Z-ADD     datcor        I95DAT
050100041015     C                   EVAL      I95CAP = VABCAD
050200041015     C                   EVAL      I95LOC = VABLOD
050300041015     C                   CALL      'TISI95R'
050400041015     C                   PARM                    TISI95DS
050500041015     C                   EVAL      VABPRD = O95PRV
050600041015     C                   ENDIF
050700041015     C                   ENDIF
050800040928     C*
050900040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT50001'
051000040928     C* NCL
051100040928     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
051200040928     C                   EXSR      CHKNUM
051300040928     C                   IF        PiInt=*on
051400040928     C                   Z-ADD     PiVal         VABNCL
051500040928     C                   ELSE
051600040928     C                   SETON                                        32
051700040928     C                   Z-ADD     *zeros        VABNCL
051800040928     C                   EVAL      vinmsg = %trimr(vinmsg)
051900040928     C                             + ' ' + 'VABNCL'
052000040928     C                   ENDIF
052100040928     C* PKB
052200040928     C                   EVAL      PiStr=%trim(%subst(vindta:64:18))
052300040928     C                   EXSR      CHKNUM
052400040928     C                   IF        PiNum=*on
052500040928     C                   Z-ADD     PiVal         VABPKB
052600040928     C                   ELSE
052700040928     C                   SETON                                        32
052800040928     C                   Z-ADD     *zeros        VABPKB
052900040928     C                   EVAL      vinmsg = %trimr(vinmsg)
053000040928     C                             + ' ' + 'VABPKB'
053100040928     C                   ENDIF
053200040928     C                   ENDIF
053300041012     C*
053400041012     C                   IF        %trim(%subst(vindta:1:4)) = 'RT70'
053500041012     C                   IF        %trim(%subst(vindta:50:2)) = '11' OR
053600041012     C                             %trim(%subst(vindta:50:2)) = '43' OR
053700041012     C                             %trim(%subst(vindta:50:2)) = '44'
053800041012     C                   EVAL      wWrtVAB = 'N'
053900041012     C                   ENDIF
054000041012     C                   ENDIF
054100040928     C*
054200040928     C                   IF        %trim(%subst(vindta:1:4)) = 'RT80'
054300040928     C                   IF        %trim(%subst(vindta:51:3)) = '50'
054400040928     C                   EVAL      FlgCAS = '1'
054500040928     C* CAS
054600040928     C                   EVAL      PiStr=%trim(%subst(vindta:54:18))
054700040928     C                   EXSR      CHKNUM
054800040928     C                   IF        PiNum=*on
054900040928     C                   Z-ADD     PiVal         VABCAS
055000040928     C                   ELSE
055100040928     C                   SETON                                        32
055200040928     C                   Z-ADD     *zeros        VABCAS
055300040928     C                   EVAL      vinmsg = %trimr(vinmsg)
055400040928     C                             + ' ' + 'VABCAS'
055500040928     C                   ENDIF
055600040928     C* VCA
055700040928     C                   EVAL      VABVCA=%trim(%subst(vindta:72:3))
055800040928     C                   ENDIF
055900040928     C                   ENDIF
056000060504     C*
056100060504     C* Reperisco il barcode sul tipo record 'RT20LS'
056200060504     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20LS'
056300060504     C                   EVAL      VATCCM = VABCCM
056400060504     C                   EVAL      VATAAS = VABAAS
056500060504     C                   EVAL      VATLNP = VABLNP
056600060504     C                   EVAL      VATNSP = VABNSP
056700060504     C                   EVAL      VATTRC = 'E'
056800060504     C                   EVAL      VATNOT=%trim(%subst(vindta:8:35))
056900060504     C                   WRITE     FIVAT000
057000060504     C                   ENDIF
057100020204     C*
057200010607     C                   ENDSR
057300010607     C*----------------------------------------------------*
057400010601
057500020204
057600020204     C*----------------------------------------------------*
057700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057800020204     C*----------------------------------------------------*
057900020204     C     CHKIMPDIV     BEGSR
058000020204     C*
058100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
058200020204     C                   Z-ADD     *zeros        wrkDec            9 9
058300020204     C*
058400020204     C* Come prima cosa effettuo considerazioni sulla divisa
058500020204     C                   IF        vabIAS > *zeros
058600020204     C                   IF        vabVAS <> 'EUR'
058700020204     C                   EVAL      vabVAS =  'ITL'
058800020204     C                   ENDIF
058900020204     C                   ENDIF
059000020204     C*
059100020204     C                   IF        vabCAS > *zeros
059200020204     C                   IF        vabVCA <> 'EUR'
059300020204     C                   EVAL      vabVCA =  'ITL'
059400020204     C                   ENDIF
059500020204     C                   ENDIF
059600020204     C*
059700020204     C                   IF        vabVMD > *zeros
059800020321     C                   IF        vabVAD <> 'EUR'
059900020204     C                   EVAL      vabVAD =  'ITL'
060000020204     C                   ENDIF
060100020204     C                   ENDIF
060200020204     C*
060300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060400020204     C                   Z-ADD     vabIAS        wrkDec
060500020204     C                   IF        wrkDec > *zeros
060600020204     C                   IF        vabVAS = 'ITL'
060700020204     C                   EVAL      vabIAS = *zeros
060800020204     C                   ENDIF
060900020204     C                   ENDIF
061000020204     C*
061100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
061200020204     C                   Z-ADD     vabCAS        wrkDec
061300020204     C                   IF        wrkDec > *zeros
061400020204     C                   IF        vabVCA = 'ITL'
061500020204     C                   EVAL      vabCAS = *zeros
061600020204     C                   ENDIF
061700020204     C                   ENDIF
061800020204     C*
061900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
062000020204     C                   Z-ADD     vabVMD        wrkDec
062100020204     C                   IF        wrkDec > *zeros
062200020204     C                   IF        vabVAD = 'ITL'
062300020204     C                   EVAL      vabVMD = *zeros
062400020204     C                   ENDIF
062500020204     C                   ENDIF
062600020204     C*
062700020204     C                   ENDSR
062800020204     C***
062900020204
063000060504
063100060504     C*----------------------------------------------------*
063200060504     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
063300060504     C*----------------------------------------------------*
063400060504     C     PREVAT        BEGSR
063500060504     C*
063600060504     C* Compongo il nome del membro da dare al FIVATWWR
063700060504     C                   eval      parmbr = vlrhdl
063800060504     C                   movel     'M'           parmbr
063900060504     C                   eval      parccm = vlrksc
064000060504     C                   eval      paropz = '1'
064100060504     C* Effettuo la chiamata al CLLE preposto
064200060504     C                   call(e)   'TITVVTC'
064300060504     C                   parm                    parccm
064400060504     C                   parm                    parmbr
064500060504     C                   parm                    paropz
064600060504     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064700060504     C                   if        %error
064800060504     C                   movel     '1'           chkcall
064900060504     C                   else
065000060504     C                   movel     '0'           chkcall
065100060504     C                   endif
065200060504     C*
065300060504     C                   ENDSR
065400060504     C*----------------------------------------------------*
065500060504
065600010330
065700010330     C*----------------------------------------------------*
065800010330     C*  CONTROLLO NUMERICITA' CAMPI
065900010330     C*----------------------------------------------------*
066000010330     C     CHKNUM        BEGSR
066100010330     C*
066200010606     C                   IF        PiDecChr = *blanks
066300040928     C                   EVAL      PiDecChr = '.'
066400010606     C                   ENDIF
066500010606     C*
066600010606     C                   CALL(e)   'ISNUMERIC'
066700010330     C                   PARM                    PiStr            30
066800010606     C                   PARM                    PiDecChr          1
066900010330     C                   PARM      *ZEROS        PiVal            30 9
067000010330     C                   PARM      '0'           PiInt             1
067100010330     C                   PARM      '0'           PiNum             1
067200010330     C                   IF        %error
067300010606     C                   EVAL      PiNum=*off
067400010330     C                   ENDIF
067500010330     C*
067600010330     C                   ENDSR
067700010330     C***
067800010330
067900010601
068000010601
068100010601
068200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
068300010601     C     repfil        BEGSR
068400010601     C*
068500010601     C                   if        invfil = *zeros and
068600010601     C                             depfil > *zeros and
068700010629     C                             (vinflg = *blanks or
068800010629     C                              vinflg = *zeros)
068900010601     C
069000010601     C                   eval      invfil = depfil
069100010601     C                   endif
069200010601     C*
069300010601     C                   if        depfil <> invfil and
069400010601     C                             invfil > *zeros
069500010601     C                   eval      flgMulti = '1'
069600010601     C                   if        vinflg = *blanks
069700010601     C                   add       1             cntNonEl
069800010601     C                   endif
069900010601     C                   endif
070000010601     C*
070100010601     C                   if        vinflg = '2'
070200010601     C                   eval      flgStato = '2'
070300010601     C                   endif
070400010601     C*
070500010601     C                   ENDSR
070600010601     C***
070700010601
070800010330
070900010330
071000010330
071100060504      /TITLE Invio dei dati al punto operativo.
071200060504     C     invio         BEGSR
071300060504     C*
071400060504     C* 1° invio FIVAT
071500060504     C                   reset                   dscmz
071600060504     C                   move      vlrpoi        cmzdst
071700060504     C                   eval      cmzfld = 'FIVATWWR'
071800060504     C                   eval      cmzmbd = vlrhdl
071900060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
072000060504     C***                if        prmfir = *blanks
072100060504     C                   eval      cmzfla = 'FIVAT00F'
072200060504     C                   eval      cmzmba = 'FIVAT00F'
072300060504     C***                else
072400060504     C***                eval      cmzfla = prmfir
072500060504     C***                eval      cmzmba = prmfir
072600060504     C***                endif
072700060504     C                   eval      cmznrr = *zeros
072800060504     C                   move      §ctrokvt      cmznrr
072900060504     C                   eval      cmzlba = vlrfl1
073000060504     C                   call(e)   'TIS711C'
073100060504     C                   parm                    dscmz
073200060504     C                   parm      *blanks       esito
073300060504     C                   if        %error
073400060504     C                             or cmzerr = '1'
073500060504     C                             or esito  = '1'
073600060504     C                   eval      wrkesito = '3'
073700060504     C                   else
073800060504     C*
073900060504     C* 2° invio FIVAB
074000060504     C                   reset                   dscmz
074100060504     C                   move      vlrpoi        cmzdst
074200060504     C                   eval      cmzfld = vlrfou
074300060504     C                   eval      cmzmbd = vlrhdl
074400060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
074500060504     C***                if        prmfir = *blanks
074600060504     C                   eval      cmzfla = 'FIVAB00F'
074700060504     C                   eval      cmzmba = 'FIVAB00F'
074800060504     C***                else
074900060504     C***                eval      cmzfla = prmfir
075000060504     C***                eval      cmzmba = prmfir
075100060504     C***                endif
075200060504     C                   eval      cmznrr = *zeros
075300060504     C                   move      §ctrok        cmznrr
075400060504     C                   eval      cmzlba = vlrfl1
075500060504     C                   call(e)   'TIS711C'
075600060504     C                   parm                    dscmz
075700060504     C                   parm      *blanks       esito
075800060504     C                   if        %error
075900060504     C                             or cmzerr = '1'
076000060504     C                             or esito  = '1'
076100060504     C                   eval      wrkesito = '3'
076200060504     C                   endif
076300060504     C                   endif
076400060504     C*
076500060504     C                   ENDSR
076600060504     C***
076700990910
076800010601
076900010601
077000010601
077100010601      /TITLE Invio dei dati al punto operativo.
077200010601     C     opeini        BEGSR
077300010601     C*
077400010601     C* Inizializzo flag e contatori operativi
077500010601     C                   movel     '0'           flgGiro           1
077600010601     C                   movel     '0'           flgMulti          1
077700010601     C                   movel     '1'           flgStato          1
077800010615     C                   movel     '0'           flgOk             1
077900010601     C                   z-add     *zeros        cntNonEl         10 0
078000010601     C                   z-add     *zeros        depfil            3 0
078100010601     C                   z-add     *zeros        invfil            3 0
078200010601     C*
078300010601     C                   ENDSR
078400010601     C***
078500010601
078600010601
078700010330
078800010330
078900000613     C     *inzsr        BEGSR
079000990910     C*
079100990910     C     *entry        plist
079200990920     C                   parm                    tivlrds
079300990921     C                   parm      wrkesito      esito
079400000724     C                   parm                    prmlit
079500000710     C                   parm                    prmfir
079600010330     C*
079700010330     C* CALCOLA LA DATA CORRENTE
079800010330     C                   time                    wn14             14 0
079900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080000010330     C                   z-add     wn8           g08dat
080100010330     C                   z-add     *zeros        g08inv
080200010330     C                   movel     '0'           g08err
080300010330     C                   call      'XSRDA8'
080400010330     C                   parm                    wlbda8
080500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
080600000613     C*
080700000613     C                   ENDSR
080800000613     C***
080900990908
081000040928     Otitve8p   E            riepilogo         2
081100990915     O                                              'Upload via Internet'
081200990915     O                                           +1 'Traduzione TIVIN00R -'
081300060504     O                                           +1 'FIVABWWR/FIVATWWR'
081400990915     O                                           +1 'Testate bolle'
081500990915     O          E            riepilogo   2
081600990915     O                       wrkdata
081700990915     O                       wrkora              +1
081800990915     O                       procname            +1
081900990915     O          E            riepilogo   2
082000990915     O                                              'Cliente..................:'
082100010605     O                       VABCCM        z     +1
082200990915     O          E            riepilogo   2
082300990920     O                                              'Riferimento Strategi.....:'
082400990920     O                       vlrhdl              +1
082500990915     O          E            riepilogo   2
082600990915     O                                              'Giusti...................:'
082700971022     O                       §CTROK        2   +  1
082800990915     O          E            riepilogo   2
082900990915     O                                              'Sbagliati e corretti.....:'
083000971022     O                       §CTRMO        2   +  1
083100990915     O          E            riepilogo   2
083200990915     O                                              'Sbagliati e scartati.....:'
083300971022     O                       §CTRNO        2   +  1
083400000613
083500040928     Otitve8ps  E            testdett          2
083600000613     O                                              'Upload via Internet'
083700000613     O                                           +1 'Traduzione TIVIN00R -'
083800060504     O                                           +1 'FIVABWWR/FIVATWWR'
083900000613     O                                           +1 'Testate bolle'
084000000616     O          E            testdett    3
084100000613     O                                           +2 'N° rec'
084200000613     O                                           +3 'Anteprima contenuto'
084300000616     O          E            rigadett    2
084400000613     O                       rrnum               +2
084500000621     O                       recko               +3
084600000616     O          E            findett     2
084700000613     O                       wrkdata
084800000613     O                       wrkora              +1
084900000613     O                       procname            +1
085000000616     O          E            findett     2
085100000613     O                                              'Cliente..................:'
085200010605     O                       VABCCM        z     +1
085300000616     O          E            findett     2
085400000613     O                                              'Riferimento Strategi.....:'
085500000613     O                       vlrhdl              +1
085600000616     O          E            findett     2
085700000613     O                                              'Giusti...................:'
085800000613     O                       §CTROK        2   +  1
085900000616     O          E            findett     2
086000000613     O                                              'Sbagliati e corretti.....:'
086100000613     O                       §CTRMO        2   +  1
086200000616     O          E            findett     2
086300000613     O                                              'Sbagliati e scartati.....:'
086400000613     O                       §CTRNO        2   +  1
086500000613** CMD - COMANDI CL
086600040928OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
086700040928OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
086800040928DLTOVR FILE(TITVE8P TITVE8PS) LVL(*)
086900010606
087000000613
