000100060504      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR (x SCHENKER Cod. 2493133)
000200990908     H dftactgrp(*yes)
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
032700041011     C* Imposto un flag che mi indica la presenza del tipo record 'RT20FF'
032800041011     C                   MOVEL     'N'           wFlgRT20FF        1
032900041011     C* Imposto un flag che mi indica se ho già elaborato almeno 1 record del file del cliente
033000041011     C                   MOVEL     'N'           wGiro             1
033100041012     C* Imposto un flag che mi indica se trattasi d bolla da considerare (condiziona write del VAB)
033200041012     C                   MOVEL     'S'           wWrtVAB           1
033300010330     C*
033400010330     C                   ENDSR
033500010330     C*----------------------------------------------------*
033600040607     C*  IMPOSTAZIONE CAMPI COSTANTI
033700010330     C*----------------------------------------------------*
033800010330     C     DEFCAM        BEGSR
033900010330     C*
034000020204     C* Inizializzo il buffer del record da scrivere
034100040723     C                   CLEAR                   FIVAB000
034200020204     C* Imposto i valori di default...
034300040928     C                   EVAL      VABCCM = 2493133
034400040928     C                   EVAL      VABLNP = 249
034500020322     C                   EVAL      VABCTR = 000
034600060504     C                   EVAL      VABCTM = '7Q'
034700020204     C* ... e poi verifico se sono stati passati come parametri
034800020204     C                   IF        vlrppt > *blanks
034900040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
035000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035100020204     C                   EXSR      CHKNUM
035200020204     C                   IF        PiInt=*on
035300020204     C                   Z-ADD     PiVal         VABCCM
035400020204     C                   ENDIF
035500040607     C                   ENDIF
035600040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
035700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035800020204     C                   EXSR      CHKNUM
035900020204     C                   IF        PiInt=*on
036000020204     C                   Z-ADD     PiVal         VABLNP
036100020204     C                   ENDIF
036200040607     C                   ENDIF
036300040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
036400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036500020204     C                   EXSR      CHKNUM
036600020204     C                   IF        PiInt=*on
036700020204     C                   Z-ADD     PiVal         VABCTR
036800020204     C                   ENDIF
036900040607     C                   ENDIF
037000020204     C                   ENDIF
037100020204     C*
037200010330     C                   ENDSR
037300010607     C*----------------------------------------------------*
037400040723     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
037500010607     C*----------------------------------------------------*
037600010607     C     IMPVAB        BEGSR
037700010607     C*
037800040928     C* Reperimento campi ALFA a seconda del tipo record
037900040928     C                   IF        %trim(%subst(vindta:1:3)) = 'SWS'
038000041011     C*
038100041011     C* Ad ogni nuova spedizione scarico il buffer d quella precedente
038200041012     C                   IF        wGiro = 'S' AND wWrtVAB = 'S'
038300041011     C* Considerazioni finali su CBO/CAS
038400041011     C                   IF        FlgCAS = '1'
038500041011     C                   EVAL      VABCBO = '4'
038600041011     C                   ELSE
038700041011     C                   EVAL      VABCBO = '1'
038800041011     C                   ENDIF
038900041011     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039000041011     C                   EXSR      CHKIMPDIV
039100041011     C* Scarico il buffer del FIVAB
039200041011     C  N31              WRITE     FIVAB000
039300041011     C                   ENDIF
039400041011     C*
039500040928     C                   exsr      inzvar
039600040928     C                   exsr      defcam
039700040928     C                   MOVEL     DATCOR        VABAAS
039800040928     C                   MOVE      DATCOR        VABMGS
039900040928     C                   SETOFF                                       3132
040000041011     C                   MOVEL     'S'           wGiro
040100040928     C                   ENDIF
040200040928     C*
040300041011     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20FF'
040400041011     C*
040500041011     C* Imposto un flag che mi indica la presenza del tipo record 'RT20FF'
040600041011     C                   MOVEL     'S'           wFlgRT20FF
040700040928     C* RMA
040800041011     C                   EVAL      VABRMA=%trim(%subst(vindta:8:10))
040900041012     C* NSP/RMN
041000041012     C                   EVAL      PiStr=%trim(%subst(vindta:11:7))
041100041012     C                   EXSR      CHKNUM
041200041012     C                   IF        PiInt=*on
041300041012     C                   Z-ADD     PiVal         VABNSP
041400041012     C                   Z-ADD     PiVal         VABRMN
041500041012     C                   ELSE
041600041012     C                   SETON                                        32
041700041012     C                   Z-ADD     *zeros        VABNSP
041800041012     C                   Z-ADD     1             VABRMN
041900041012     C                   EVAL      vinmsg = %trimr(vinmsg)
042000041012     C                             + ' ' + 'VABNSP VABRMN'
042100041012     C                   ENDIF
042200041011     C                   ENDIF
042300040928     C*
042400041011     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20AEG'
042500041011     C* RMA
042600041011     C                   IF        wFlgRT20FF = 'N'
042700041011     C                   EVAL      VABRMA=%trim(%subst(vindta:8:10))
042800041011     C                   ENDIF
042900041011     C* NSP/RMN
043000041012     C                   EVAL      PiStr=%trim(%subst(vindta:8:10))
043100041011     C                   EXSR      CHKNUM
043200041011     C                   IF        PiInt=*on
043300041011     C                   Z-ADD     PiVal         VABNSP
043400041011     C                   Z-ADD     PiVal         VABRMN
043500041011     C                   ELSE
043600041012     C* NSP/RMN
043700041012     C                   EVAL      PiStr=%trim(%subst(vindta:12:6))
043800041012     C                   EXSR      CHKNUM
043900041012     C                   IF        PiInt=*on
044000041012     C                   Z-ADD     PiVal         VABNSP
044100041012     C                   Z-ADD     PiVal         VABRMN
044200041012     C                   ELSE
044300041012     C                   SETON                                        32
044400041012     C                   Z-ADD     *zeros        VABNSP
044500041012     C                   Z-ADD     1             VABRMN
044600041012     C                   EVAL      vinmsg = %trimr(vinmsg)
044700041012     C                             + ' ' + 'VABNSP VABRMN'
044800041012     C                   ENDIF
044900041011     C                   ENDIF
045000040928     C                   ENDIF
045100040928     C*
045200040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40CN'
045300040928     C* RSD
045400040928     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
045500040928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045600040928     C     '@':'A'       XLATE     VABRSD        VABRSD
045700040928     C* ==
045800040928     C* RD2
045900040928     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
046000040928     C* IND
046100040928     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
046200040928     C* NT2
046300040928     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
046400040928     C* LOD
046500040928     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
046600040928     C* CAD
046700040928     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
046800040928     C                   EXSR      CHKNUM
046900040928     C                   IF        PiInt=*on
047000040928     C                   Z-ADD     PiVal         Num5_0
047100040928     C                   MOVEL(p)  Num5_0        VABCAD
047200040928     C                   ELSE
047300040928     C                   SETON                                        32
047400040928     C                   EVAL      VABCAD = *zeros
047500040928     C                   EVAL      vinmsg = %trimr(vinmsg)
047600040928     C                             + ' ' + 'VABCAD'
047700040928     C                   ENDIF
047800040928     C* Reperisco la provincia dal CAP e dalla località
047900040928     C                   IF        VABCAD <> *blanks
048000040928     C                   CLEAR                   TISI95DS
048100040928     C                   EVAL      I95TCN = '3'
048200040928     C                   Z-ADD     datcor        I95DAT
048300040928     C                   EVAL      I95CAP = VABCAD
048400040928     C                   EVAL      I95LOC = VABLOD
048500040928     C                   CALL      'TISI95R'
048600040928     C                   PARM                    TISI95DS
048700040928     C                   EVAL      VABPRD = O95PRV
048800040928     C                   ENDIF
048900040928     C                   ENDIF
049000041015     C*
049100041015     C* Se presente il tipo record 'RT40DP' by-passa il tipo record 'RT40CN'...  x cui:
049200041015     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40DP'
049300041015     C* RSD
049400041015     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
049500041015     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
049600041015     C     '@':'A'       XLATE     VABRSD        VABRSD
049700041015     C* ==
049800041015     C* RD2
049900041015     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
050000041015     C* IND
050100041015     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
050200041015     C* NT2
050300041015     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
050400041015     C* LOD
050500041015     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
050600041015     C* CAD
050700041015     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
050800041015     C                   EXSR      CHKNUM
050900041015     C                   IF        PiInt=*on
051000041015     C                   Z-ADD     PiVal         Num5_0
051100041015     C                   MOVEL(p)  Num5_0        VABCAD
051200041015     C                   ELSE
051300041015     C                   SETON                                        32
051400041015     C                   EVAL      VABCAD = *zeros
051500041015     C                   EVAL      vinmsg = %trimr(vinmsg)
051600041015     C                             + ' ' + 'VABCAD'
051700041015     C                   ENDIF
051800041015     C* Reperisco la provincia dal CAP e dalla località
051900041015     C                   IF        VABCAD <> *blanks
052000041015     C                   CLEAR                   TISI95DS
052100041015     C                   EVAL      I95TCN = '3'
052200041015     C                   Z-ADD     datcor        I95DAT
052300041015     C                   EVAL      I95CAP = VABCAD
052400041015     C                   EVAL      I95LOC = VABLOD
052500041015     C                   CALL      'TISI95R'
052600041015     C                   PARM                    TISI95DS
052700041015     C                   EVAL      VABPRD = O95PRV
052800041015     C                   ENDIF
052900041015     C                   ENDIF
053000040928     C*
053100040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT50001'
053200040928     C* NCL
053300040928     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
053400040928     C                   EXSR      CHKNUM
053500040928     C                   IF        PiInt=*on
053600040928     C                   Z-ADD     PiVal         VABNCL
053700040928     C                   ELSE
053800040928     C                   SETON                                        32
053900040928     C                   Z-ADD     *zeros        VABNCL
054000040928     C                   EVAL      vinmsg = %trimr(vinmsg)
054100040928     C                             + ' ' + 'VABNCL'
054200040928     C                   ENDIF
054300040928     C* PKB
054400040928     C                   EVAL      PiStr=%trim(%subst(vindta:64:18))
054500040928     C                   EXSR      CHKNUM
054600040928     C                   IF        PiNum=*on
054700040928     C                   Z-ADD     PiVal         VABPKB
054800040928     C                   ELSE
054900040928     C                   SETON                                        32
055000040928     C                   Z-ADD     *zeros        VABPKB
055100040928     C                   EVAL      vinmsg = %trimr(vinmsg)
055200040928     C                             + ' ' + 'VABPKB'
055300040928     C                   ENDIF
055400040928     C                   ENDIF
055500041012     C*
055600041012     C                   IF        %trim(%subst(vindta:1:4)) = 'RT70'
055700041012     C                   IF        %trim(%subst(vindta:50:2)) = '11' OR
055800041012     C                             %trim(%subst(vindta:50:2)) = '43' OR
055900041012     C                             %trim(%subst(vindta:50:2)) = '44'
056000041012     C                   EVAL      wWrtVAB = 'N'
056100041012     C                   ENDIF
056200041012     C                   ENDIF
056300040928     C*
056400040928     C                   IF        %trim(%subst(vindta:1:4)) = 'RT80'
056500040928     C                   IF        %trim(%subst(vindta:51:3)) = '50'
056600040928     C                   EVAL      FlgCAS = '1'
056700040928     C* CAS
056800040928     C                   EVAL      PiStr=%trim(%subst(vindta:54:18))
056900040928     C                   EXSR      CHKNUM
057000040928     C                   IF        PiNum=*on
057100040928     C                   Z-ADD     PiVal         VABCAS
057200040928     C                   ELSE
057300040928     C                   SETON                                        32
057400040928     C                   Z-ADD     *zeros        VABCAS
057500040928     C                   EVAL      vinmsg = %trimr(vinmsg)
057600040928     C                             + ' ' + 'VABCAS'
057700040928     C                   ENDIF
057800040928     C* VCA
057900040928     C                   EVAL      VABVCA=%trim(%subst(vindta:72:3))
058000040928     C                   ENDIF
058100040928     C                   ENDIF
058200060504     C*
058300060504     C* Reperisco il barcode sul tipo record 'RT20LS'
058400060504     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20LS'
058500060504     C                   EVAL      VATCCM = VABCCM
058600060504     C                   EVAL      VATAAS = VABAAS
058700060504     C                   EVAL      VATLNP = VABLNP
058800060504     C                   EVAL      VATNSP = VABNSP
058900060504     C                   EVAL      VATTRC = 'E'
059000060504     C                   EVAL      VATNOT=%trim(%subst(vindta:8:35))
059100060504     C                   WRITE     FIVAT000
059200060504     C                   ENDIF
059300020204     C*
059400010607     C                   ENDSR
059500010607     C*----------------------------------------------------*
059600010601
059700020204
059800020204     C*----------------------------------------------------*
059900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060000020204     C*----------------------------------------------------*
060100020204     C     CHKIMPDIV     BEGSR
060200020204     C*
060300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060400020204     C                   Z-ADD     *zeros        wrkDec            9 9
060500020204     C*
060600020204     C* Come prima cosa effettuo considerazioni sulla divisa
060700020204     C                   IF        vabIAS > *zeros
060800020204     C                   IF        vabVAS <> 'EUR'
060900020204     C                   EVAL      vabVAS =  'ITL'
061000020204     C                   ENDIF
061100020204     C                   ENDIF
061200020204     C*
061300020204     C                   IF        vabCAS > *zeros
061400020204     C                   IF        vabVCA <> 'EUR'
061500020204     C                   EVAL      vabVCA =  'ITL'
061600020204     C                   ENDIF
061700020204     C                   ENDIF
061800020204     C*
061900020204     C                   IF        vabVMD > *zeros
062000020321     C                   IF        vabVAD <> 'EUR'
062100020204     C                   EVAL      vabVAD =  'ITL'
062200020204     C                   ENDIF
062300020204     C                   ENDIF
062400020204     C*
062500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062600020204     C                   Z-ADD     vabIAS        wrkDec
062700020204     C                   IF        wrkDec > *zeros
062800020204     C                   IF        vabVAS = 'ITL'
062900020204     C                   EVAL      vabIAS = *zeros
063000020204     C                   ENDIF
063100020204     C                   ENDIF
063200020204     C*
063300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
063400020204     C                   Z-ADD     vabCAS        wrkDec
063500020204     C                   IF        wrkDec > *zeros
063600020204     C                   IF        vabVCA = 'ITL'
063700020204     C                   EVAL      vabCAS = *zeros
063800020204     C                   ENDIF
063900020204     C                   ENDIF
064000020204     C*
064100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
064200020204     C                   Z-ADD     vabVMD        wrkDec
064300020204     C                   IF        wrkDec > *zeros
064400020204     C                   IF        vabVAD = 'ITL'
064500020204     C                   EVAL      vabVMD = *zeros
064600020204     C                   ENDIF
064700020204     C                   ENDIF
064800020204     C*
064900020204     C                   ENDSR
065000020204     C***
065100020204
065200060504
065300060504     C*----------------------------------------------------*
065400060504     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
065500060504     C*----------------------------------------------------*
065600060504     C     PREVAT        BEGSR
065700060504     C*
065800060504     C* Compongo il nome del membro da dare al FIVATWWR
065900060504     C                   eval      parmbr = vlrhdl
066000060504     C                   movel     'M'           parmbr
066100060504     C                   eval      parccm = vlrksc
066200060504     C                   eval      paropz = '1'
066300060504     C* Effettuo la chiamata al CLLE preposto
066400060504     C                   call(e)   'TITVVTC'
066500060504     C                   parm                    parccm
066600060504     C                   parm                    parmbr
066700060504     C                   parm                    paropz
066800060504     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066900060504     C                   if        %error
067000060504     C                   movel     '1'           chkcall
067100060504     C                   else
067200060504     C                   movel     '0'           chkcall
067300060504     C                   endif
067400060504     C*
067500060504     C                   ENDSR
067600060504     C*----------------------------------------------------*
067700060504
067800010330
067900010330     C*----------------------------------------------------*
068000010330     C*  CONTROLLO NUMERICITA' CAMPI
068100010330     C*----------------------------------------------------*
068200010330     C     CHKNUM        BEGSR
068300010330     C*
068400010606     C                   IF        PiDecChr = *blanks
068500040928     C                   EVAL      PiDecChr = '.'
068600010606     C                   ENDIF
068700010606     C*
068800010606     C                   CALL(e)   'ISNUMERIC'
068900010330     C                   PARM                    PiStr            30
069000010606     C                   PARM                    PiDecChr          1
069100010330     C                   PARM      *ZEROS        PiVal            30 9
069200010330     C                   PARM      '0'           PiInt             1
069300010330     C                   PARM      '0'           PiNum             1
069400010330     C                   IF        %error
069500010606     C                   EVAL      PiNum=*off
069600010330     C                   ENDIF
069700010330     C*
069800010330     C                   ENDSR
069900010330     C***
070000010330
070100010601
070200010601
070300010601
070400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
070500010601     C     repfil        BEGSR
070600010601     C*
070700010601     C                   if        invfil = *zeros and
070800010601     C                             depfil > *zeros and
070900010629     C                             (vinflg = *blanks or
071000010629     C                              vinflg = *zeros)
071100010601     C
071200010601     C                   eval      invfil = depfil
071300010601     C                   endif
071400010601     C*
071500010601     C                   if        depfil <> invfil and
071600010601     C                             invfil > *zeros
071700010601     C                   eval      flgMulti = '1'
071800010601     C                   if        vinflg = *blanks
071900010601     C                   add       1             cntNonEl
072000010601     C                   endif
072100010601     C                   endif
072200010601     C*
072300010601     C                   if        vinflg = '2'
072400010601     C                   eval      flgStato = '2'
072500010601     C                   endif
072600010601     C*
072700010601     C                   ENDSR
072800010601     C***
072900010601
073000010330
073100010330
073200010330
073300060504      /TITLE Invio dei dati al punto operativo.
073400060504     C     invio         BEGSR
073500060504     C*
073600060504     C* 1° invio FIVAT
073700060504     C                   reset                   dscmz
073800060504     C                   move      vlrpoi        cmzdst
073900060504     C                   eval      cmzfld = 'FIVATWWR'
074000060504     C                   eval      cmzmbd = vlrhdl
074100060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
074200060504     C***                if        prmfir = *blanks
074300060504     C                   eval      cmzfla = 'FIVAT00F'
074400060504     C                   eval      cmzmba = 'FIVAT00F'
074500060504     C***                else
074600060504     C***                eval      cmzfla = prmfir
074700060504     C***                eval      cmzmba = prmfir
074800060504     C***                endif
074900060504     C                   eval      cmznrr = *zeros
075000060504     C                   move      §ctrokvt      cmznrr
075100060504     C                   eval      cmzlba = vlrfl1
075200060504     C                   call(e)   'TIS711C'
075300060504     C                   parm                    dscmz
075400060504     C                   parm      *blanks       esito
075500060504     C                   if        %error
075600060504     C                             or cmzerr = '1'
075700060504     C                             or esito  = '1'
075800060504     C                   eval      wrkesito = '3'
075900060504     C                   else
076000060504     C*
076100060504     C* 2° invio FIVAB
076200060504     C                   reset                   dscmz
076300060504     C                   move      vlrpoi        cmzdst
076400060504     C                   eval      cmzfld = vlrfou
076500060504     C                   eval      cmzmbd = vlrhdl
076600060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
076700060504     C***                if        prmfir = *blanks
076800060504     C                   eval      cmzfla = 'FIVAB00F'
076900060504     C                   eval      cmzmba = 'FIVAB00F'
077000060504     C***                else
077100060504     C***                eval      cmzfla = prmfir
077200060504     C***                eval      cmzmba = prmfir
077300060504     C***                endif
077400060504     C                   eval      cmznrr = *zeros
077500060504     C                   move      §ctrok        cmznrr
077600060504     C                   eval      cmzlba = vlrfl1
077700060504     C                   call(e)   'TIS711C'
077800060504     C                   parm                    dscmz
077900060504     C                   parm      *blanks       esito
078000060504     C                   if        %error
078100060504     C                             or cmzerr = '1'
078200060504     C                             or esito  = '1'
078300060504     C                   eval      wrkesito = '3'
078400060504     C                   endif
078500060504     C                   endif
078600060504     C*
078700060504     C                   ENDSR
078800060504     C***
078900990910
079000010601
079100010601
079200010601
079300010601      /TITLE Invio dei dati al punto operativo.
079400010601     C     opeini        BEGSR
079500010601     C*
079600010601     C* Inizializzo flag e contatori operativi
079700010601     C                   movel     '0'           flgGiro           1
079800010601     C                   movel     '0'           flgMulti          1
079900010601     C                   movel     '1'           flgStato          1
080000010615     C                   movel     '0'           flgOk             1
080100010601     C                   z-add     *zeros        cntNonEl         10 0
080200010601     C                   z-add     *zeros        depfil            3 0
080300010601     C                   z-add     *zeros        invfil            3 0
080400010601     C*
080500010601     C                   ENDSR
080600010601     C***
080700010601
080800010601
080900010330
081000010330
081100000613     C     *inzsr        BEGSR
081200990910     C*
081300990910     C     *entry        plist
081400990920     C                   parm                    tivlrds
081500990921     C                   parm      wrkesito      esito
081600000724     C                   parm                    prmlit
081700000710     C                   parm                    prmfir
081800010330     C*
081900010330     C* CALCOLA LA DATA CORRENTE
082000010330     C                   time                    wn14             14 0
082100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
082200010330     C                   z-add     wn8           g08dat
082300010330     C                   z-add     *zeros        g08inv
082400010330     C                   movel     '0'           g08err
082500010330     C                   call      'XSRDA8'
082600010330     C                   parm                    wlbda8
082700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
082800000613     C*
082900000613     C                   ENDSR
083000000613     C***
083100990908
083200040928     Otitve8p   E            riepilogo         2
083300990915     O                                              'Upload via Internet'
083400990915     O                                           +1 'Traduzione TIVIN00R -'
083500060504     O                                           +1 'FIVABWWR/FIVATWWR'
083600990915     O                                           +1 'Testate bolle'
083700990915     O          E            riepilogo   2
083800990915     O                       wrkdata
083900990915     O                       wrkora              +1
084000990915     O                       procname            +1
084100990915     O          E            riepilogo   2
084200990915     O                                              'Cliente..................:'
084300010605     O                       VABCCM        z     +1
084400990915     O          E            riepilogo   2
084500990920     O                                              'Riferimento Strategi.....:'
084600990920     O                       vlrhdl              +1
084700990915     O          E            riepilogo   2
084800990915     O                                              'Giusti...................:'
084900971022     O                       §CTROK        2   +  1
085000990915     O          E            riepilogo   2
085100990915     O                                              'Sbagliati e corretti.....:'
085200971022     O                       §CTRMO        2   +  1
085300990915     O          E            riepilogo   2
085400990915     O                                              'Sbagliati e scartati.....:'
085500971022     O                       §CTRNO        2   +  1
085600000613
085700040928     Otitve8ps  E            testdett          2
085800000613     O                                              'Upload via Internet'
085900000613     O                                           +1 'Traduzione TIVIN00R -'
086000060504     O                                           +1 'FIVABWWR/FIVATWWR'
086100000613     O                                           +1 'Testate bolle'
086200000616     O          E            testdett    3
086300000613     O                                           +2 'N° rec'
086400000613     O                                           +3 'Anteprima contenuto'
086500000616     O          E            rigadett    2
086600000613     O                       rrnum               +2
086700000621     O                       recko               +3
086800000616     O          E            findett     2
086900000613     O                       wrkdata
087000000613     O                       wrkora              +1
087100000613     O                       procname            +1
087200000616     O          E            findett     2
087300000613     O                                              'Cliente..................:'
087400010605     O                       VABCCM        z     +1
087500000616     O          E            findett     2
087600000613     O                                              'Riferimento Strategi.....:'
087700000613     O                       vlrhdl              +1
087800000616     O          E            findett     2
087900000613     O                                              'Giusti...................:'
088000000613     O                       §CTROK        2   +  1
088100000616     O          E            findett     2
088200000613     O                                              'Sbagliati e corretti.....:'
088300000613     O                       §CTRMO        2   +  1
088400000616     O          E            findett     2
088500000613     O                                              'Sbagliati e scartati.....:'
088600000613     O                       §CTRNO        2   +  1
088700000613** CMD - COMANDI CL
088800040928OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
088900040928OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVE8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
089000040928DLTOVR FILE(TITVE8P TITVE8PS) LVL(*)
089100010606
089200000613
