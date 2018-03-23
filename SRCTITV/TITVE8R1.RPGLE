000100060504      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR (x SCHENKER Cod. 2493133)
000200101105     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040723     FFIVABwwr  O    E             DISK    usropn
000600060504     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500040607     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100060504     D parccm          s              8    INZ(*blanks)
002200060504     D parmbr          s             10    INZ(*blanks)
002300060504     D paropz          s              1    INZ(*blanks)
002400060504     D chkcall         s              1    INZ(*blanks)
002500100706
002600100706
002700100706     D*------------------
002800100706     D* DS REPERIMENTO NUMERATORE
002900100706     D*------------------
003000100706     D trul33ds      e ds                  inz
003100100706     D*------------------
003200100706     D* DS ARCHITETTURA
003300100706     D*------------------
003400100706     D kpjba         e ds                  inz
003500100706
003600010330
003700010330
003800010330
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200010601     C*
004300010601     C                   exsr      opeini
004400010605     C                   exsr      rwvab
004500060504     C*
004600060504     C* Effettuo la chiamata al CLLE preposto
004700060504     C                   call(e)   'TITVVTC'
004800060504     C                   parm                    parccm
004900060504     C                   parm                    parmbr
005000060504     C                   parm      '2'           paropz
005100040928     C*
005200040928     C* Lancio TISI95R solo x chiusura
005300040928     C                   CLEAR                   TISI95DS
005400040928     C                   EVAL      I95TLA = 'C'
005500040928     C                   CALL      'TISI95R'
005600040928     C                   PARM                    TISI95DS
005700010601     C*
005800010601     C                   seton                                        lr
005900010601
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006500010601     C*--------------------------------------------------------
006600010601     C     PREELA        BEGSR
006700010601     C*
006800010601     C* SE OCCORRE SPEDIRE IN FILIALE
006900010601     C                   if        invfil <> *zeros and
007000010601     C                             flgGiro = '0'
007100010601     C*
007200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007300010601     C                   eval      flgGiro = '1'
007400010601     C*
007500010601     C                   endif
007600010601     C*
007700010601     C                   ENDSR
007800010601     C***
007900010601
008000010601
008100010601
008200010601
008300010601
008400010601
008500010601
008600010601     C*--------------------------------------------------------
008700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008800010601     C*--------------------------------------------------------
008900010601     C     ENDELA        BEGSR
009000000616     C*
009100010601     C                   ENDSR
009200010601     C***
009300000613
009400010330
009500010601
009600010601
009700010601
009800010330     C*--------------------------------------------------------
009900040723     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010000010330     C*--------------------------------------------------------
010100010605     C     RWVAB         BEGSR
010200010330     C*
010300010330     C                   if        not %open(tivin00r)
010400010330     C                   open      tivin00r
010500010330     C                   endif
010600040723     C                   if        not %open(fivabwwr)
010700040723     C                   open      fivabwwr
010800010330     C                   endif
010900060504     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011000060504     C                   exsr      prevat
011100060504     C*
011200060504     C                   if        chkcall = '0'
011300060504     C*
011400060504     C                   if        not %open(fivatwwr)
011500060504     C                   open      fivatwwr
011600060504     C                   endif
011700010330     C*
011800060504     C                   clear                   §CTROK
011900060504     C                   clear                   §CTROKVT          5 0
012000010604     C                   clear                   §CTRMO
012100010604     C                   clear                   §CTRNO
012200010330     C*
012300010330     C                   DO        *HIVAL
012400010330     C*
012500010330     C                   READ      tivin00r                               70
012600010618     C*
012700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012800010618     C                   if        vinflg = '1'
012900010618     C                   eval      flgOk = '1'
013000010618     C                   endif
013100010618     C*
013200010330     C                   if        vindta > *blanks
013300010330     C                   add       1             rrnum
013400010330     C*
013500010601     C                   if        *in70 = *off and
013600010330     C                             (vinflg = *blanks
013700010330     C                              or vinflg = '0'
013800010330     C                              or vinflg = '2')
013900010330     C*
014000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014100010711     C                   if        vinflg = *blanks or vinflg = '0'
014200010711     C                   clear                   vinmsg
014300010711     C                   endif
014400010601     C*
014500010605     C                   exsr      impvab
014600010601     C*
014700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
014800010605     C                   eval      depfil = VABLNP
014900010601     C                   exsr      repfil
015000010601     C                   if        depfil = invfil
015100021025     C                   if        vlrpoi = 999
015200021025     C                   MOVE(P)   invfil        VABFGS
015300021025     C                   else
015400021025     C                   MOVE(P)   vlrpoi        VABFGS
015500021025     C                   endif
015600010601     C*
015700010601     C                   exsr      PREELA
015800010601     C*
015900010604     C* Ebbene...
016000010604     C*
016100010604     C  N31              ADD       1             §CTROK            7 0
016200010604     C   32              ADD       1             §CTRMO            7 0
016300010604     C   31              ADD       1             §CTRNO            7 0
016400010604     C*
016500010604     C                   if        *in31 = *off and
016600010604     C                             *in32 = *off
016700010604     C                   eval      vinflg = '1'
016800010604     C                   else
016900010604     C                   eval      vinflg = '2'
017000010604     C                   endif
017100010604     C                   endif
017200010601     C*
017300010604     C                   endif
017400010604     C*
017500010330     C                   else
017600010330     C                   eval      vinflg = '1'
017700010330     C                   endif
017800010601     C*
017900010601     C  N70              update    tivin000
018000010330     C*
018100010330     C  N70              ENDdo
018200100824     C*
018300100824     C* Scarico il buffer del FIVAB rimasto eventualmente "in canna"
018400100824     C                   EXSR      WRIVAB
018500060504     C*
018600060504     C                   endif                                                  * chkcall
018700010601     C*
018800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018900010601     C                   if        cntNonEl = *zeros or
019000010601     C                             flgMulti = '0'
019100010330     C* Se non ci sono record con errori ...
019200010601     C                   if        §ctrno = 0 and
019300010604     C                             §ctrmo = 0 and
019400010601     C                             flgStato <> '2'
019500010330     C* ... restituisco esito OK.
019600010330     C                   eval      wrkesito = '0'
019700010330     C                   else
019800010330     C                   if        §ctrok > 0
019900010330     C                   eval      wrkesito = '1'
020000010330     C                   else
020100010615     C                   if        flgOk = '0'
020200010615     C                   eval      wrkesito = '2'
020300010615     C                   else
020400010615     C                   eval      wrkesito = '6'
020500010615     C                   endif
020600010330     C                   endif
020700010330     C                   endif
020800010601     C                   else
020900010601     C                   eval      wrkesito = '9'
021000010601     C                   endif
021100010330     C*
021200010330     C                   if        %open(tivin00r)
021300010330     C                   close     tivin00r
021400010330     C                   endif
021500040723     C                   if        %open(fivabwwr)
021600040723     C                   close     fivabwwr
021700010330     C                   endif
021800060504     C                   if        %open(fivatwwr)
021900060504     C                   close     fivatwwr
022000060504     C                   endif
022100010601     C*
022200010601     C                   if        vlrpoi <> 999
022300010601     C                   eval      invfil = vlrpoi
022400010601     C                   endif
022500010330     C*
022600010330     C                   if        §ctrok > 0
022700010601     C                             and invfil > *zeros
022800010330     C                   exsr      invio
022900010330     C                   endif
023000010601     C*
023100010618     C                   if        flgGiro = '1'
023200010601     C                   exsr      endela
023300010618     C                   endif
023400010330     C*
023500010330     C                   ENDSR
023600010330     C***
023700010330
023800010601
023900010601
024000010601
024100010601
024200010330     C*----------------------------------------------------*
024300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024400010330     C*----------------------------------------------------*
024500040607     C     INZVAR        BEGSR
024600010330     C*
024700020204     C                   Z-ADD     *zeros        Num5_0            5 0
024800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024900041011     C                   MOVEL     '0'           FlgCAS            1
025000041011     C* Imposto un flag che mi indica se ho già elaborato almeno 1 record del file del cliente
025100041011     C                   MOVEL     'N'           wGiro             1
025200041012     C* Imposto un flag che mi indica se trattasi d bolla da considerare (condiziona write del VAB)
025300041012     C                   MOVEL     'S'           wWrtVAB           1
025400010330     C*
025500010330     C                   ENDSR
025600010330     C*----------------------------------------------------*
025700040607     C*  IMPOSTAZIONE CAMPI COSTANTI
025800010330     C*----------------------------------------------------*
025900010330     C     DEFCAM        BEGSR
026000010330     C*
026100020204     C* Inizializzo il buffer del record da scrivere
026200040723     C                   CLEAR                   FIVAB000
026300020204     C* Imposto i valori di default...
026400100706     C                   EVAL      VABCCM = 2494479
026500040928     C                   EVAL      VABLNP = 249
026600100831     C                   EVAL      VABCTR = 001
026700060504     C                   EVAL      VABCTM = '7Q'
026800020204     C* ... e poi verifico se sono stati passati come parametri
026900020204     C                   IF        vlrppt > *blanks
027000040607     C                   IF        %subst(vlrppt:1:7) <> *blanks
027100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027200020204     C                   EXSR      CHKNUM
027300020204     C                   IF        PiInt=*on
027400020204     C                   Z-ADD     PiVal         VABCCM
027500020204     C                   ENDIF
027600040607     C                   ENDIF
027700040607     C                   IF        %subst(vlrppt:8:3) <> *blanks
027800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027900020204     C                   EXSR      CHKNUM
028000020204     C                   IF        PiInt=*on
028100020204     C                   Z-ADD     PiVal         VABLNP
028200020204     C                   ENDIF
028300040607     C                   ENDIF
028400040607     C                   IF        %subst(vlrppt:11:3) <> *blanks
028500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028600020204     C                   EXSR      CHKNUM
028700020204     C                   IF        PiInt=*on
028800020204     C                   Z-ADD     PiVal         VABCTR
028900020204     C                   ENDIF
029000040607     C                   ENDIF
029100020204     C                   ENDIF
029200020204     C*
029300010330     C                   ENDSR
029400010607     C*----------------------------------------------------*
029500040723     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029600010607     C*----------------------------------------------------*
029700010607     C     IMPVAB        BEGSR
029800010607     C*
029900040928     C* Reperimento campi ALFA a seconda del tipo record
030000040928     C                   IF        %trim(%subst(vindta:1:3)) = 'SWS'
030100041011     C*
030200041011     C* Ad ogni nuova spedizione scarico il buffer d quella precedente
030300100824     C                   EXSR      WRIVAB
030400041011     C*
030500040928     C                   exsr      inzvar
030600040928     C                   exsr      defcam
030700040928     C                   MOVEL     DATCOR        VABAAS
030800040928     C                   MOVE      DATCOR        VABMGS
030900100706     C                   SETOFF                                       3132
031000100706     C                   MOVEL     'S'           wGiro
031100100706     C*
031200100706     C* ......VABNSP/VATNSP
031300100706     C* NSP => Stacco un numeratore da AZNUM
031400100706     C                   clear                   TRUL33DS
031500100706     C                   eval      I33OPE = *zeros
031600100706     C                   eval      I33CNU = 302
031700100706     C                   eval      I33NUM = 1
031800100706     C                   movel     TRUL33DS      KPJBU
031900100706     C                   call      'TRUL33R'
032000100706     C                   parm                    KPJBA
032100100706     C                   movel     KPJBU         TRUL33DS
032200100706     C                   if        O33ERR = *zeros
032300100706     C                   z-add     O33NRF        VABNSP
032400100706     C                   z-add     O33NRF        VATNSP
032500100706     C                   else
032600100706     C                   seton                                        31
032700100706     C                   EVAL      vinmsg = %trimr(vinmsg)
032800100706     C                             + ' ' + 'VABNSP VATNSP'
032900100706     C                   endif
033000100706     C*
033100040928     C                   ENDIF
033200040928     C*
033300100706     C* Tipo record => RT20FF
033400041011     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20FF'
033500040928     C* RMA
033600100706     C                   EVAL      VABRMA=%trim(%subst(vindta:8:35))
033700100706     C                   ENDIF
033800040928     C*
033900100706     C* Tipo record => RT20AEG
034000101105     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20AEG'
034100101105     C* RMN
034200110412     C                   IF        %subst(vindta:51:8) <> *blanks
034300101105     C                   EVAL      PiStr=%trim(%subst(vindta:51:8))
034400101105     C                   EXSR      CHKNUM
034500101105     C                   IF        PiInt=*on
034600101105     C                   Z-ADD     PiVal         VABRMN
034700101105     C                   ELSE
034800101105     C                   SETON                                        32
034900101105     C                   Z-ADD     1             VABRMN
035000101105     C                   EVAL      vinmsg = %trimr(vinmsg)
035100101105     C                             + ' ' + 'VABRMN'
035200101105     C                   ENDIF
035300040928     C                   ENDIF
035400110412     C                   ENDIF
035500040928     C*
035600100706     C* Tipo record => RT40CN
035700040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40CN'
035800040928     C* RSD
035900040928     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
036000040928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036100040928     C     '@':'A'       XLATE     VABRSD        VABRSD
036200040928     C* ==
036300040928     C* RD2
036400040928     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
036500040928     C* IND
036600040928     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
036700040928     C* NT2
036800040928     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
036900040928     C* LOD
037000040928     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
037100040928     C* CAD
037200040928     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
037300040928     C                   EXSR      CHKNUM
037400040928     C                   IF        PiInt=*on
037500040928     C                   Z-ADD     PiVal         Num5_0
037600040928     C                   MOVEL(p)  Num5_0        VABCAD
037700040928     C                   ELSE
037800040928     C                   SETON                                        32
037900040928     C                   EVAL      VABCAD = *zeros
038000040928     C                   EVAL      vinmsg = %trimr(vinmsg)
038100040928     C                             + ' ' + 'VABCAD'
038200040928     C                   ENDIF
038300040928     C* Reperisco la provincia dal CAP e dalla località
038400040928     C                   IF        VABCAD <> *blanks
038500040928     C                   CLEAR                   TISI95DS
038600040928     C                   EVAL      I95TCN = '3'
038700040928     C                   Z-ADD     datcor        I95DAT
038800040928     C                   EVAL      I95CAP = VABCAD
038900040928     C                   EVAL      I95LOC = VABLOD
039000040928     C                   CALL      'TISI95R'
039100040928     C                   PARM                    TISI95DS
039200040928     C                   EVAL      VABPRD = O95PRV
039300040928     C                   ENDIF
039400040928     C                   ENDIF
039500041015     C*
039600100706     C* Tipo record => RT40DP
039700041015     C* Se presente il tipo record 'RT40DP' by-passa il tipo record 'RT40CN'...  x cui:
039800041015     C                   IF        %trim(%subst(vindta:1:7)) = 'RT40DP'
039900041015     C* RSD
040000041015     C                   EVAL      VABRSD=%trim(%subst(vindta:28:35))
040100041015     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
040200041015     C     '@':'A'       XLATE     VABRSD        VABRSD
040300041015     C* ==
040400041015     C* RD2
040500041015     C                   EVAL      VABRD2=%trim(%subst(vindta:63:35))
040600041015     C* IND
040700041015     C                   EVAL      VABIND=%trim(%subst(vindta:98:35))
040800041015     C* NT2
040900041015     C                   EVAL      VABNT2=%trim(%subst(vindta:133:35))
041000041015     C* LOD
041100041015     C                   EVAL      VABLOD=%trim(%subst(vindta:168:35))
041200041015     C* CAD
041300041015     C                   EVAL      PiStr=%trim(%subst(vindta:203:5))
041400041015     C                   EXSR      CHKNUM
041500041015     C                   IF        PiInt=*on
041600041015     C                   Z-ADD     PiVal         Num5_0
041700041015     C                   MOVEL(p)  Num5_0        VABCAD
041800041015     C                   ELSE
041900041015     C                   SETON                                        32
042000041015     C                   EVAL      VABCAD = *zeros
042100041015     C                   EVAL      vinmsg = %trimr(vinmsg)
042200041015     C                             + ' ' + 'VABCAD'
042300041015     C                   ENDIF
042400041015     C* Reperisco la provincia dal CAP e dalla località
042500041015     C                   IF        VABCAD <> *blanks
042600041015     C                   CLEAR                   TISI95DS
042700041015     C                   EVAL      I95TCN = '3'
042800041015     C                   Z-ADD     datcor        I95DAT
042900041015     C                   EVAL      I95CAP = VABCAD
043000041015     C                   EVAL      I95LOC = VABLOD
043100041015     C                   CALL      'TISI95R'
043200041015     C                   PARM                    TISI95DS
043300041015     C                   EVAL      VABPRD = O95PRV
043400041015     C                   ENDIF
043500041015     C                   ENDIF
043600040928     C*
043700100706     C* Tipo record => RT50001
043800040928     C                   IF        %trim(%subst(vindta:1:7)) = 'RT50001'
043900040928     C* NCL
044000040928     C                   EVAL      PiStr=%trim(%subst(vindta:8:8))
044100040928     C                   EXSR      CHKNUM
044200040928     C                   IF        PiInt=*on
044300040928     C                   Z-ADD     PiVal         VABNCL
044400040928     C                   ELSE
044500040928     C                   SETON                                        32
044600040928     C                   Z-ADD     *zeros        VABNCL
044700040928     C                   EVAL      vinmsg = %trimr(vinmsg)
044800040928     C                             + ' ' + 'VABNCL'
044900040928     C                   ENDIF
045000040928     C* PKB
045100040928     C                   EVAL      PiStr=%trim(%subst(vindta:64:18))
045200040928     C                   EXSR      CHKNUM
045300040928     C                   IF        PiNum=*on
045400040928     C                   Z-ADD     PiVal         VABPKB
045500040928     C                   ELSE
045600040928     C                   SETON                                        32
045700040928     C                   Z-ADD     *zeros        VABPKB
045800040928     C                   EVAL      vinmsg = %trimr(vinmsg)
045900040928     C                             + ' ' + 'VABPKB'
046000040928     C                   ENDIF
046100040928     C                   ENDIF
046200041012     C*
046300100706     C* Tipo record => RT70
046400100824     C***                IF        %trim(%subst(vindta:1:4)) = 'RT70'
046500100824     C***                IF        %trim(%subst(vindta:50:2)) = '11' OR
046600100824     C***                          %trim(%subst(vindta:50:2)) = '43' OR
046700100824     C***                          %trim(%subst(vindta:50:2)) = '44'
046800100824     C***                EVAL      wWrtVAB = 'N'
046900100824     C***                ENDIF
047000100824     C***                ENDIF
047100100824     C*
047200100824     C* Tipo record => RT10785
047300100824     C                   IF        %trim(%subst(vindta:1:7)) = 'RT10785'
047400100824     C                   EVAL      wWrtVAB = 'N'
047500100824     C                   ENDIF
047600040928     C*
047700100706     C* Tipo record => RT80
047800040928     C                   IF        %trim(%subst(vindta:1:4)) = 'RT80'
047900040928     C                   IF        %trim(%subst(vindta:51:3)) = '50'
048000040928     C                   EVAL      FlgCAS = '1'
048100110406     C* - gestione particolarità x traffico 2494613
048200110406     C                   IF        VABCCM = 2494613
048300110406     C                   EVAL      VABTIC = 'OM'
048400110406     C                   EVAL      VABRMO = 'BOUGIES LA FRANCAISE'
048500110406     C                   ENDIF
048600111018     C* - gestione particolarità x traffico 2494678
048700111018     C                   IF        VABCCM = 2494678
048800111018     C                   EVAL      VABTIC = 'OM'
048900111018     C                   EVAL      VABRMO = 'ABT'
049000111018     C                   ENDIF
049100040928     C* CAS
049200040928     C                   EVAL      PiStr=%trim(%subst(vindta:54:18))
049300040928     C                   EXSR      CHKNUM
049400040928     C                   IF        PiNum=*on
049500040928     C                   Z-ADD     PiVal         VABCAS
049600040928     C                   ELSE
049700040928     C                   SETON                                        32
049800040928     C                   Z-ADD     *zeros        VABCAS
049900040928     C                   EVAL      vinmsg = %trimr(vinmsg)
050000040928     C                             + ' ' + 'VABCAS'
050100040928     C                   ENDIF
050200040928     C* VCA
050300040928     C                   EVAL      VABVCA=%trim(%subst(vindta:72:3))
050400040928     C                   ENDIF
050500040928     C                   ENDIF
050600060504     C*
050700100706     C* Tipo record => RT20LS
050800060504     C* Reperisco il barcode sul tipo record 'RT20LS'
050900100824     C                   IF        %trim(%subst(vindta:1:7)) = 'RT20LS' AND
051000100824     C                             wWrtVAB = 'S'
051100100706     C                   EVAL      VATFGS = VABFGS
051200060504     C                   EVAL      VATCCM = VABCCM
051300060504     C                   EVAL      VATAAS = VABAAS
051400060504     C                   EVAL      VATLNP = VABLNP
051500060504     C                   EVAL      VATNSP = VABNSP
051600060504     C                   EVAL      VATTRC = 'E'
051700100707     C                   EVAL      VATNOT='00'+%trim(%subst(vindta:8:35))
051800110421     C* - gestione particolarità x traffico 2494613
051900110421     C                   IF        VATCCM = 2494613
052000110421     C                   EVAL      VATNOT=%trim(%subst(vindta:8:35))
052100110421     C                   ENDIF
052200060504     C                   WRITE     FIVAT000
052300060504     C                   ENDIF
052400020204     C*
052500010607     C                   ENDSR
052600010607     C*----------------------------------------------------*
052700100824
052800100824
052900100824     C*----------------------------------------------------*
053000100824     C*  SCARICO BUFFER FILE TESTATA - FNVAB
053100100824     C*----------------------------------------------------*
053200100824     C     WRIVAB        BEGSR
053300100824     C*
053400100824     C                   IF        wGiro = 'S' AND wWrtVAB = 'S'
053500100824     C* Considerazioni finali su CBO/CAS
053600100824     C                   IF        FlgCAS = '1'
053700100824     C                   EVAL      VABCBO = '4'
053800100824     C                   ELSE
053900100824     C                   EVAL      VABCBO = '1'
054000100824     C                   ENDIF
054100100824     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054200100824     C                   EXSR      CHKIMPDIV
054300100824     C* Scarico il buffer del FIVAB
054400100824     C  N31              WRITE     FIVAB000
054500100824     C                   ENDIF
054600100824     C*
054700100824     C                   ENDSR
054800100824     C***
054900010601
055000020204
055100020204     C*----------------------------------------------------*
055200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055300020204     C*----------------------------------------------------*
055400020204     C     CHKIMPDIV     BEGSR
055500020204     C*
055600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055700020204     C                   Z-ADD     *zeros        wrkDec            9 9
055800020204     C*
055900020204     C* Come prima cosa effettuo considerazioni sulla divisa
056000020204     C                   IF        vabIAS > *zeros
056100020204     C                   IF        vabVAS <> 'EUR'
056200020204     C                   EVAL      vabVAS =  'ITL'
056300020204     C                   ENDIF
056400020204     C                   ENDIF
056500020204     C*
056600020204     C                   IF        vabCAS > *zeros
056700020204     C                   IF        vabVCA <> 'EUR'
056800020204     C                   EVAL      vabVCA =  'ITL'
056900020204     C                   ENDIF
057000020204     C                   ENDIF
057100020204     C*
057200020204     C                   IF        vabVMD > *zeros
057300020321     C                   IF        vabVAD <> 'EUR'
057400020204     C                   EVAL      vabVAD =  'ITL'
057500020204     C                   ENDIF
057600020204     C                   ENDIF
057700020204     C*
057800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057900020204     C                   Z-ADD     vabIAS        wrkDec
058000020204     C                   IF        wrkDec > *zeros
058100020204     C                   IF        vabVAS = 'ITL'
058200020204     C                   EVAL      vabIAS = *zeros
058300020204     C                   ENDIF
058400020204     C                   ENDIF
058500020204     C*
058600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
058700020204     C                   Z-ADD     vabCAS        wrkDec
058800020204     C                   IF        wrkDec > *zeros
058900020204     C                   IF        vabVCA = 'ITL'
059000020204     C                   EVAL      vabCAS = *zeros
059100020204     C                   ENDIF
059200020204     C                   ENDIF
059300020204     C*
059400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059500020204     C                   Z-ADD     vabVMD        wrkDec
059600020204     C                   IF        wrkDec > *zeros
059700020204     C                   IF        vabVAD = 'ITL'
059800020204     C                   EVAL      vabVMD = *zeros
059900020204     C                   ENDIF
060000020204     C                   ENDIF
060100020204     C*
060200020204     C                   ENDSR
060300020204     C***
060400020204
060500060504
060600060504     C*----------------------------------------------------*
060700060504     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060800060504     C*----------------------------------------------------*
060900060504     C     PREVAT        BEGSR
061000060504     C*
061100060504     C* Compongo il nome del membro da dare al FIVATWWR
061200060504     C                   eval      parmbr = vlrhdl
061300060504     C                   movel     'M'           parmbr
061400060504     C                   eval      parccm = vlrksc
061500060504     C                   eval      paropz = '1'
061600060504     C* Effettuo la chiamata al CLLE preposto
061700060504     C                   call(e)   'TITVVTC'
061800060504     C                   parm                    parccm
061900060504     C                   parm                    parmbr
062000060504     C                   parm                    paropz
062100060504     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062200060504     C                   if        %error
062300060504     C                   movel     '1'           chkcall
062400060504     C                   else
062500060504     C                   movel     '0'           chkcall
062600060504     C                   endif
062700060504     C*
062800060504     C                   ENDSR
062900060504     C*----------------------------------------------------*
063000060504
063100010330
063200010330     C*----------------------------------------------------*
063300010330     C*  CONTROLLO NUMERICITA' CAMPI
063400010330     C*----------------------------------------------------*
063500010330     C     CHKNUM        BEGSR
063600010330     C*
063700010606     C                   IF        PiDecChr = *blanks
063800040928     C                   EVAL      PiDecChr = '.'
063900010606     C                   ENDIF
064000010606     C*
064100010606     C                   CALL(e)   'ISNUMERIC'
064200010330     C                   PARM                    PiStr            30
064300010606     C                   PARM                    PiDecChr          1
064400010330     C                   PARM      *ZEROS        PiVal            30 9
064500010330     C                   PARM      '0'           PiInt             1
064600010330     C                   PARM      '0'           PiNum             1
064700010330     C                   IF        %error
064800010606     C                   EVAL      PiNum=*off
064900010330     C                   ENDIF
065000010330     C*
065100010330     C                   ENDSR
065200010330     C***
065300010330
065400010601
065500010601
065600010601
065700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
065800010601     C     repfil        BEGSR
065900010601     C*
066000010601     C                   if        invfil = *zeros and
066100010601     C                             depfil > *zeros and
066200010629     C                             (vinflg = *blanks or
066300010629     C                              vinflg = *zeros)
066400010601     C
066500010601     C                   eval      invfil = depfil
066600010601     C                   endif
066700010601     C*
066800010601     C                   if        depfil <> invfil and
066900010601     C                             invfil > *zeros
067000010601     C                   eval      flgMulti = '1'
067100010601     C                   if        vinflg = *blanks
067200010601     C                   add       1             cntNonEl
067300010601     C                   endif
067400010601     C                   endif
067500010601     C*
067600010601     C                   if        vinflg = '2'
067700010601     C                   eval      flgStato = '2'
067800010601     C                   endif
067900010601     C*
068000010601     C                   ENDSR
068100010601     C***
068200010601
068300010330
068400010330
068500010330
068600060504      /TITLE Invio dei dati al punto operativo.
068700060504     C     invio         BEGSR
068800060504     C*
068900060504     C* 1° invio FIVAT
069000060504     C                   reset                   dscmz
069100060504     C                   move      vlrpoi        cmzdst
069200060504     C                   eval      cmzfld = 'FIVATWWR'
069300060504     C                   eval      cmzmbd = vlrhdl
069400060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
069500060504     C***                if        prmfir = *blanks
069600060504     C                   eval      cmzfla = 'FIVAT00F'
069700060504     C                   eval      cmzmba = 'FIVAT00F'
069800060504     C***                else
069900060504     C***                eval      cmzfla = prmfir
070000060504     C***                eval      cmzmba = prmfir
070100060504     C***                endif
070200060504     C                   eval      cmznrr = *zeros
070300060504     C                   move      §ctrokvt      cmznrr
070400060504     C                   eval      cmzlba = vlrfl1
070500060504     C                   call(e)   'TIS711C'
070600060504     C                   parm                    dscmz
070700060504     C                   parm      *blanks       esito
070800060504     C                   if        %error
070900060504     C                             or cmzerr = '1'
071000060504     C                             or esito  = '1'
071100060504     C                   eval      wrkesito = '3'
071200060504     C                   else
071300060504     C*
071400060504     C* 2° invio FIVAB
071500060504     C                   reset                   dscmz
071600060504     C                   move      vlrpoi        cmzdst
071700060504     C                   eval      cmzfld = vlrfou
071800060504     C                   eval      cmzmbd = vlrhdl
071900060504     C                   eval      %subst(cmzmbd:1:1) = 'M'
072000060504     C***                if        prmfir = *blanks
072100060504     C                   eval      cmzfla = 'FIVAB00F'
072200060504     C                   eval      cmzmba = 'FIVAB00F'
072300060504     C***                else
072400060504     C***                eval      cmzfla = prmfir
072500060504     C***                eval      cmzmba = prmfir
072600060504     C***                endif
072700060504     C                   eval      cmznrr = *zeros
072800060504     C                   move      §ctrok        cmznrr
072900060504     C                   eval      cmzlba = vlrfl1
073000060504     C                   call(e)   'TIS711C'
073100060504     C                   parm                    dscmz
073200060504     C                   parm      *blanks       esito
073300060504     C                   if        %error
073400060504     C                             or cmzerr = '1'
073500060504     C                             or esito  = '1'
073600060504     C                   eval      wrkesito = '3'
073700060504     C                   endif
073800060504     C                   endif
073900060504     C*
074000060504     C                   ENDSR
074100060504     C***
074200990910
074300010601
074400010601
074500010601
074600010601      /TITLE Invio dei dati al punto operativo.
074700010601     C     opeini        BEGSR
074800010601     C*
074900010601     C* Inizializzo flag e contatori operativi
075000010601     C                   movel     '0'           flgGiro           1
075100010601     C                   movel     '0'           flgMulti          1
075200010601     C                   movel     '1'           flgStato          1
075300010615     C                   movel     '0'           flgOk             1
075400010601     C                   z-add     *zeros        cntNonEl         10 0
075500010601     C                   z-add     *zeros        depfil            3 0
075600010601     C                   z-add     *zeros        invfil            3 0
075700010601     C*
075800010601     C                   ENDSR
075900010601     C***
076000010601
076100010601
076200010330
076300010330
076400000613     C     *inzsr        BEGSR
076500990910     C*
076600990910     C     *entry        plist
076700990920     C                   parm                    tivlrds
076800990921     C                   parm      wrkesito      esito
076900000724     C                   parm                    prmlit
077000000710     C                   parm                    prmfir
077100010330     C*
077200010330     C* CALCOLA LA DATA CORRENTE
077300100706     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077400100706     C                   eval      datcor = %dec(%date() : *iso)
077500000613     C*
077600000613     C                   ENDSR
077700000613     C***
