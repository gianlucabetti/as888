000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120920
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070719     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500000830
002600041025     D*------------------
002700041025     D* DS REPERIMENTO NUMERATORE
002800041025     D*------------------
002900041025     D trul33ds      e ds                  inz
003000041025     D*------------------
003100041025     D* DS ARCHITETTURA
003200041025     D*------------------
003300041025     D kpjba         e ds                  inz
003400041025     D*------------------
003500990908
003600010201
003700010201
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100000613     C*
004200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004300070719     C*
004400070719     C* Esegue lancio TISI95R solo x chiusura
004500070719     C                   CLEAR                   TISI95DS
004600070719     C                   EVAL      I95TLA = 'C'
004700070719     C                   CALL      'TISI95R'
004800070719     C                   PARM                    TISI95DS
004900000613     C*
005000010202     C* Effettuo la chiamata al CLLE preposto
005100040506     C                   call(e)   'TITVVTC'
005200010202     C                   parm                    parccm
005300010202     C                   parm                    parmbr
005400010202     C                   parm      '2'           paropz
005500010202     C*
005600000616     C*
005700010201     C                   seton                                        LR
005800000613
005900990908
006000000801
006100910830     C*--------------------------------------------------------
006200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006300910830     C*--------------------------------------------------------
006400040526     C     RWFILE        BEGSR
006500990910     C*
006600990914     C                   if        not %open(tivin00r)
006700990908     C                   open      tivin00r
006800990914     C                   endif
006900021113     C                   if        not %open(fivabwwr)
007000021113     C                   open      fivabwwr
007100990914     C                   endif
007200120228
007300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007400020305     C                   exsr      prevat
007500010201     C*
007600010202     C                   if        chkcall = '0'
007700010202     C*
007800021113     C                   if        not %open(fivatwwr)
007900021113     C                   open      fivatwwr
008000010201     C                   endif
008100990910     C*
008200010201     C                   clear                   §CTROKVB          5 0
008300020305     C                   clear                   §CTROKVT          5 0
008400000801     C                   clear                   §CTRMO            5 0
008500000801     C                   clear                   §CTRNO            5 0
008600040910     C*
008700921023     C                   DO        *HIVAL
008800990913     C*
008900990915     C                   READ      tivin00r                               70
009000040910     C                   if        vindta > *blanks
009100000613     C                   add       1             rrnum
009200000801     C*
009300000801     C                   if        *in70 = *off
009400000801     C                             and
009500000801     C                             (vinflg = *blanks
009600000801     C                              or vinflg = '0'
009700000801     C                              or vinflg = '2')
009800000801     C*
009900000801     C                   clear                   vinmsg
010000000801     C                   eval      vinflg = '1'
010100040910     C*
010200040910     C* Eseguo routine d traduzione
010300040910     C                   exsr      impvabvat
010400040802     C*
010500010305     C                   endif
010600000905     C*
010700000905     C                   else
010800000905     C                   eval      vinflg = '1'
010900000905     C                   endif
011000000905     C*
011100000905     C  N70              update    tivin000
011200000905     C*
011300991022     C  N70              ENDdo
011400010202     C*
011500010202     C                   endif
011600990910
011700990910     C* Se non ci sono record con errori ...
011800000710     C                   if        §ctrno = 0
011900990910     C* ... restituisco esito OK.
012000990921     C                   eval      wrkesito = '0'
012100990910     C                   else
012200010201     C                   if        §ctrokvb > 0
012300990921     C                   eval      wrkesito = '1'
012400000710     C                   else
012500000710     C                   eval      wrkesito = '2'
012600990910     C                   endif
012700000710     C                   endif
012800990910     C*
012900990914     C                   if        %open(tivin00r)
013000990908     C                   close     tivin00r
013100990914     C                   endif
013200021113     C                   if        %open(fivabwwr)
013300021113     C                   close     fivabwwr
013400990914     C                   endif
013500021113     C                   if        %open(fivatwwr)
013600021113     C                   close     fivatwwr
013700010201     C                   endif
013800990910     C*
013900010201     C                   if        §ctrokvb > 0
014000000724     C                             and vlrpoi <> *zeros
014100010202     C                   exsr      invio
014200990920     C                   endif
014300990920     C*
014400910830     C                   ENDSR
014500000613     C***
014600990920
014700000801     C*----------------------------------------------------*
014800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014900000801     C*----------------------------------------------------*
015000010201     C     INZVAR        BEGSR
015100000801     C*
015200040802     C                   Z-ADD     *zeros        Num5_0            5 0
015300040802     C                   MOVEL     '0'           FlgCAS            1
015400000801     C*
015500000801     C                   ENDSR
015600000801     C*----------------------------------------------------*
015700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
015800000801     C*----------------------------------------------------*
015900000801     C     DEFCAM        BEGSR
016000000801     C*
016100021113     C                   CLEAR                   FIVAB000
016200040802     C                   CLEAR                   FIVAT000
016300020619     C* Imposto i valori di default...
016400120227     C                   Z-ADD     0056548       VABCCM
016500120227     C                   Z-ADD     0056548       VATCCM
016600070719     C                   Z-ADD     005           VABLNP
016700070719     C                   Z-ADD     005           VATLNP
016800070531     C                   Z-ADD     000           VABCTR
016900070531     C                   MOVEL     '7Q'          VABCTM
017000040823     C                   MOVEL     '1'           VABCBO
017100020619     C* ... e poi verifico se sono stati passati come parametri
017200020619     C                   IF        vlrppt > *blanks
017300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017500020619     C                   EXSR      CHKNUM
017600020619     C                   IF        PiInt=*on
017700020619     C                   Z-ADD     PiVal         VABCCM
017800020619     C                   Z-ADD     PiVal         VATCCM
017900020619     C                   ENDIF
018000040506     C                   ENDIF
018100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018300020619     C                   EXSR      CHKNUM
018400020619     C                   IF        PiInt=*on
018500020619     C                   Z-ADD     PiVal         VABLNP
018600020619     C                   Z-ADD     PiVal         VATLNP
018700040506     C                   ENDIF
018800020619     C                   ENDIF
018900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019100020619     C                   EXSR      CHKNUM
019200020619     C                   IF        PiInt=*on
019300020619     C                   Z-ADD     PiVal         VABCTR
019400040506     C                   ENDIF
019500020619     C                   ENDIF
019600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
019700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
019800060202     C                   ENDIF
019900020619     C                   ENDIF
020000000801     C*
020100000801     C                   ENDSR
020200000801     C*----------------------------------------------------*
020300040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020400000801     C*----------------------------------------------------*
020500040910     C     IMPVABVAT     BEGSR
020600040910     C*
020700040910     C* Traduzione relativa ai tipi record del file del cliente
020800040910     C*
020900040910     C***
021000120227     C* ...tipo record 'A' (testata)
021100120227     C                   IF        %subst(vindta:49:1) = 'A'
021200070719     C* Resetto indicatore d anomalia sul singolo record
021300070719     C                   eval      vinflg = '1'
021400040910     C* ......inizializzazioni iniziali e formati record file Bartolini
021500040910     C                   EXSR      INZVAR
021600040910     C                   EXSR      DEFCAM
021700040928     C*
021800040928     C                   Z-ADD     *zeros        errore            1 0
021900040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
022000040928     C                   MOVEL     datcor        VABAAS
022100040928     C                   MOVEL     datcor        VATAAS
022200040928     C                   MOVE      datcor        VABMGS
022300040928     C                   MOVE(P)   vlrpoi        VABFGS
022400040928     C                   MOVE(P)   vlrpoi        VATFGS
022500120228
022600120228     C* Reperimento campi ALFA
022700120228
022800120228     C* Considerazioni sulla ragione sociale del destinatario da indicare
022900120228     C                   EVAL      VABRSD=%trim(%subst(vindta:668:35))
023000120228     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
023100120228     C     '@':'A'       XLATE     VABRSD        VABRSD
023200120228     C* ==
023300120228     C                   EVAL      VABRD2=%trim(%subst(vindta:668+35:50-35))
023400120228     C                   EVAL      VABIND=%trim(%subst(vindta:550:30))
023500120228     C                   EVAL      VABLOD=%trim(%subst(vindta:718:30))
023600120228     C                   EVAL      VABPRD=%trim(%subst(vindta:786:2))
023700120228     C                   EVAL      VABRMA=%trim(%subst(vindta:118:10))
023800120228     C                   EVAL      VABNOT=%trim(%subst(vindta:142:35))
023900120228     C                   EVAL      VABNT2=%trim(%subst(vindta:142+35:60-35))
024000120228     C                   IF        %subst(vindta:649:2) = 'IT'
024100120228     C                   EVAL      VABNZD=*blanks
024200120228     C                   ELSE
024300120228     C                   EVAL      VABNZD=%trim(%subst(vindta:649:3))
024400120228     C                   ENDIF
024500120228     C                   IF        %subst(vindta:202:1) = 'R'
024600120228     C                   EVAL      VABCBO='2'
024700120228     C                   ELSE
024800120228     C                   EVAL      VABCBO='1'
024900120228     C                   ENDIF
025000120228     C                   IF        %subst(vindta:1489:1) = '1'
025100120228     C                   eval      VABFFD = 'S'
025200120228     C                   ENDIF
025300120228     C                   IF        %subst(vindta:1489:1) = '2'
025400120228     C                   eval      VABTC1 = 'A'
025500120228     C                   ENDIF
025600120228
025700120228     C* Reperimento campi NUMERICI
025800120228
025900041025     C* NSP => Stacco un numeratore da AZNUM
026000041025     C                   clear                   TRUL33DS
026100041025     C                   eval      I33OPE = *zeros
026200041025     C                   eval      I33CNU = 302
026300041025     C                   eval      I33NUM = 1
026400041025     C                   movel     TRUL33DS      KPJBU
026500041025     C                   call      'TRUL33R'
026600041025     C                   parm                    KPJBA
026700041025     C                   movel     KPJBU         TRUL33DS
026800041025     C                   if        O33ERR = *zeros
026900041025     C                   z-add     O33NRF        VABNSP
027000041025     C                   z-add     O33NRF        VATNSP
027100041025     C                   else
027200041025     C                   Z-ADD     1             errore
027300041025     C                   EVAL      vinmsg = %trimr(vinmsg)
027400041025     C                             + ' ' + 'VABNSP VATNSP'
027500041025     C                   endif
027600120228     C* RMN
027700120227     C                   EVAL      PiStr=%trim(%subst(vindta:118:10))
027800060202     C                   EXSR      CHKNUM
027900060202     C                   IF        PiInt=*on
028000060202     C                   Z-ADD     PiVal         VABRMN
028100060202     C                   ELSE
028200060202     C                   ADD       1             errore
028300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028400060202     C                             + ' ' + 'VABRMN'
028500060202     C                   ENDIF
028600120228     C* NCL
028700120227     C                   EVAL      PiStr=%trim(%subst(vindta:70:5))
028800060202     C                   EXSR      CHKNUM
028900060202     C                   IF        PiInt=*on
029000060202     C                   Z-ADD     PiVal         VABNCL
029100060202     C                   ELSE
029200060202     C                   ADD       1             errore
029300060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029400060202     C                             + ' ' + 'VABNCL'
029500060202     C                   ENDIF
029600120228     C* PKB
029700120228     C                   EVAL      PiStr=%trim(%subst(vindta:55:8))
029800060202     C                   EXSR      CHKNUM
029900060202     C                   IF        PiNum=*on
030000070726     C                   EVAL      PiVal = PiVal / 1000                         da grammi a kg
030100070726     C                   Z-ADD(H)  PiVal         VABPKB
030200060202     C                   ELSE
030300060202     C                   ADD       1             errore
030400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
030500060202     C                             + ' ' + 'VABPKB'
030600060202     C                   ENDIF
030700120228     C* CAD
030800120228     C                   EVAL      PiStr=%trim(%subst(vindta:640:9))
030900060202     C                   EXSR      CHKNUM
031000060202     C                   IF        PiInt=*on
031100060202     C                   Z-ADD     PiVal         Num5_0
031200060202     C                   MOVEL(P)  Num5_0        VABCAD
031300060202     C                   ELSE
031400060202     C                   ADD       1             errore
031500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
031600060202     C                             + ' ' + 'VABCAD'
031700060202     C                   ENDIF
031800120228     C* CAS
031900120228     C                   IF        %trim(%subst(vindta:1523:13)) <> *blanks and
032000120228     C                             %trim(%subst(vindta:1523:13)) <> *zeros
032100120228     C                   EVAL      VABVCA = %trim(%subst(vindta:1536:3))
032200120228     C                   EVAL      PiStr=%trim(%subst(vindta:1523:13))
032300120228     C                   EXSR      CHKNUM
032400120228     C                   IF        PiNum=*on
032500120228     C                   EVAL      PiVal = PiVal / 100                          * gest. 2 decimali
032600120228     C                   Z-ADD     PiVal         VABCAS
032700120228     C                   eval      FlgCAS = '1'
032800120228     C                   ELSE
032900120228     C                   ADD       1             errore
033000120228     C                   Z-ADD     *zeros        VABCAS
033100120228     C                   EVAL      vinmsg = %trimr(vinmsg)
033200120228     C                             + ' ' + 'VABCAS'
033300120228     C                   ENDIF
033400120228     C                   ENDIF
033500120228     C*
033600120228     C* Carico l'estensioni B e I del FNVAT
033700120228     C                   exsr      exevatb
033800120228     C                   exsr      exevati
033900120228
034000060202     C*
034100060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
034200120228
034300120228     C* TIC
034400120912     C*** OBSOLETO
034500120228      * se c/assegno > di 500 euri tipo incasso CM
034600120912     C***                IF        VABCAS > 500
034700120912     c***                eval      VABTIC = 'CM'
034800120912     c***                else
034900120912     c***                clear                   VABTIC
035000120912     c***                endif
035100120912     C* il tipo incasso contrassegno è sempre BM
035200120920     C* se c/assegno > di 0
035300120920     C                   IF        VABCAS > 0
035400120912     c                   eval      VABTIC = 'BM'
035500120920     c                   else
035600120920     c                   clear                   VABTIC
035700120920     c                   endif
035800120228
035900120228      * PRD
036000120228     C* Reperisco la provincia dal CAP e dalla località
036100120228     C                   IF        VABPRD  = *blanks AND
036200120228     C                             VABCAD <> *blanks
036300120228     C                   CLEAR                   TISI95DS
036400120228     C                   EVAL      I95TCN = '3'
036500120228     C                   Z-ADD     datcor        I95DAT
036600120228     C                   EVAL      I95CAP = VABCAD
036700120228     C                   EVAL      I95LOC = VABLOD
036800120228     C                   CALL      'TISI95R'
036900120228     C                   PARM                    TISI95DS
037000120228     C                   EVAL      VABPRD = O95PRV
037100120228     C                   ENDIF
037200120228
037300120228      * CBO
037400060202     C                   IF        FlgCAS <> '0'
037500060202     C                   IF        VABCBO = '1'
037600060202     C                   EVAL      VABCBO = '4'
037700060202     C                   ELSE
037800060202     C                   EVAL      VABCBO = '6'
037900060202     C                   ENDIF
038000060202     C                   ENDIF
038100060202     C*
038200060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038300060202     C                   EXSR      CHKIMPDIV
038400060202     C* ...e in fine...
038500060202     C                   WRITE     FIVAB000
038600040910     C*
038700040910     C                   ENDIF
038800040910     C***
038900120228     C* ...tipo record 'B' (dettaglio barcode segnacooli)
039000120228     C                   IF        %subst(vindta:49:1) = 'B'
039100070719     C* Resetto indicatore d anomalia sul singolo record
039200070719     C                   eval      vinflg = '1'
039300120228     C*
039400040910     C                   EVAL      VATTRC = 'E'
039500120228     C                   EVAL      VATNOT=%trim(%subst(vindta:56:24))
039600120228     C                   exsr      wrivat                                       => scarico VAT
039700040910     C*
039800040910     C                   ENDIF
039900010202     C*
040000000801     C* Ebbene...
040100000801     C                   ADD       1             §CTRMO
040200010201     C                   IF        errore <> *zeros
040300000801     C                   ADD       1             §CTRNO
040400000801     C                   EVAL      vinflg = '2'
040500000801     C                   ELSE
040600010201     C                   ADD       1             §CTROKVB
040700000801     C                   ENDIF
040800000801     C*
040900000801     C                   ENDSR
041000120228     C*----------------------------------------------------*
041100120228     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
041200120228     C*----------------------------------------------------*
041300120228     C     EXEVATB       BEGSR
041400120228     C*
041500120228     C                   EVAL      VATTRC='B'
041600120228     C                   if        %subst(vindta:652:16) <> *blanks and
041700120228     C                             %subst(vindta:652:16) <> *zeros
041800120228     C                   EVAL      VATNOT=%trim(%subst(vindta:652:16))
041900120228     c                   endif
042000120228     C*
042100120228     C                   exsr      wrivat                                       => scarico VAT
042200120228     C*
042300120228     C                   ENDSR
042400120228     C*----------------------------------------------------*
042500120228     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "I"
042600120228     C*----------------------------------------------------*
042700120228     C     EXEVATI       BEGSR
042800120228     C*
042900120228     C                   IF        %subst(vindta:1548:1) = '1'
043000120228     C                   EVAL      VATTRC='I'
043100120228     C                   EVAL      VATNOT=%trim(%subst(vindta:1549:35))
043200120228     C                   exsr      wrivat                                       => scarico VAT
043300120228     C                   EVAL      VATTRC='J'
043400120228     C                   EVAL      VATNOT=%trim(%subst(vindta:1549+35:50-35))
043500120228     C                   exsr      wrivat                                       => scarico VAT
043600120228     C                   ENDIF
043700120228     C*
043800120228     C                   ENDSR
043900120228     C*----------------------------------------------------*
044000120228     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044100120228     C*----------------------------------------------------*
044200120228     C     WRIVAT        BEGSR
044300120228     C*
044400120228     C* Scrivo solo se valorizzato qualcosa
044500120228     C                   IF        VATNOT <> *blanks
044600120228     C                   WRITE     FIVAT000
044700120228     C                   ENDIF
044800120228     C*
044900120228     C                   ENDSR
045000010202     C*----------------------------------------------------*
045100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045200010202     C*----------------------------------------------------*
045300020305     C     PREVAT        BEGSR
045400010202     C*
045500021113     C* Compongo il nome del membro da dare al FIVATWWR
045600010202     C                   eval      parmbr = vlrhdl
045700010202     C                   movel     'M'           parmbr
045800060113     C                   eval      parccm = vlrksc
045900010202     C                   eval      paropz = '1'
046000010202     C* Effettuo la chiamata al CLLE preposto
046100040506     C                   call(e)   'TITVVTC'
046200010202     C                   parm                    parccm
046300010202     C                   parm                    parmbr
046400010202     C                   parm                    paropz
046500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046600010202     C                   if        %error
046700010202     C                   movel     '1'           chkcall
046800010202     C                   else
046900010202     C                   movel     '0'           chkcall
047000010202     C                   endif
047100010202     C*
047200010202     C                   ENDSR
047300000801     C*----------------------------------------------------*
047400000801     C*  CONTROLLO NUMERICITA' CAMPI
047500000801     C*----------------------------------------------------*
047600000801     C     CHKNUM        BEGSR
047700000801     C*
047800000801     C                   call(e)   'ISNUMERIC'
047900000801     C                   PARM                    PiStr            30
048000040714     C                   PARM      ','           PiDecChr          1
048100000801     C                   PARM      *ZEROS        PiVal            30 9
048200000801     C                   PARM      '0'           PiInt             1
048300000801     C                   PARM      '0'           PiNum             1
048400000801     C                   IF        %error
048500000801     C                   EVAL      PiInt=*off
048600000801     C                   ENDIF
048700000801     C*
048800000801     C                   ENDSR
048900000801     C***
049000000801
049100011113
049200011113     C*----------------------------------------------------*
049300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049400011113     C*----------------------------------------------------*
049500011113     C     CHKIMPDIV     BEGSR
049600011113     C*
049700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049800011113     C                   Z-ADD     *zeros        wrkDec            9 9
049900011113     C*
050000011113     C* Come prima cosa effettuo considerazioni sulla divisa
050100011113     C                   IF        vabIAS > *zeros
050200011113     C                   IF        vabVAS <> 'EUR'
050300011113     C                   EVAL      vabVAS =  'ITL'
050400011113     C                   ENDIF
050500011113     C                   ENDIF
050600011113     C*
050700011113     C                   IF        vabCAS > *zeros
050800011113     C                   IF        vabVCA <> 'EUR'
050900011113     C                   EVAL      vabVCA =  'ITL'
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C                   IF        vabVMD > *zeros
051400020305     C                   IF        vabVAD <> 'EUR'
051500011113     C                   EVAL      vabVAD =  'ITL'
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052000011113     C                   Z-ADD     vabIAS        wrkDec
052100011113     C                   IF        wrkDec > *zeros
052200011113     C                   IF        vabVAS = 'ITL'
052300011113     C                   EVAL      vabIAS = *zeros
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052800011113     C                   Z-ADD     vabCAS        wrkDec
052900011113     C                   IF        wrkDec > *zeros
053000011113     C                   IF        vabVCA = 'ITL'
053100011113     C                   EVAL      vabCAS = *zeros
053200011113     C                   ENDIF
053300011113     C                   ENDIF
053400011113     C*
053500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053600011113     C                   Z-ADD     vabVMD        wrkDec
053700011113     C                   IF        wrkDec > *zeros
053800011113     C                   IF        vabVAD = 'ITL'
053900011113     C                   EVAL      vabVMD = *zeros
054000011113     C                   ENDIF
054100011113     C                   ENDIF
054200011113     C*
054300011113     C                   ENDSR
054400011113     C***
054500011113
054600011113
054700000801
054800000801
054900990920      /TITLE Invio dei dati al punto operativo.
055000010202     C     invio         BEGSR
055100990920     C*
055200021113     C* 1° invio FIVAT
055300010201     C                   reset                   dscmz
055400010201     C                   move      vlrpoi        cmzdst
055500021113     C                   eval      cmzfld = 'FIVATWWR'
055600010201     C                   eval      cmzmbd = vlrhdl
055700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055800021009     C***                if        prmfir = *blanks
055900021113     C                   eval      cmzfla = 'FIVAT00F'
056000021113     C                   eval      cmzmba = 'FIVAT00F'
056100021009     C***                else
056200021009     C***                eval      cmzfla = prmfir
056300021009     C***                eval      cmzmba = prmfir
056400021009     C***                endif
056500010201     C                   eval      cmznrr = *zeros
056600020305     C                   move      §ctrokvt      cmznrr
056700021018     C                   eval      cmzlba = vlrfl1
056800010201     C                   call(e)   'TIS711C'
056900010201     C                   parm                    dscmz
057000010201     C                   parm      *blanks       esito
057100010205     C                   if        %error
057200010205     C                             or cmzerr = '1'
057300010205     C                             or esito  = '1'
057400010205     C                   eval      wrkesito = '3'
057500010205     C                   else
057600010201     C*
057700021113     C* 2° invio FIVAB
057800010201     C                   reset                   dscmz
057900010201     C                   move      vlrpoi        cmzdst
058000010201     C                   eval      cmzfld = vlrfou
058100010201     C                   eval      cmzmbd = vlrhdl
058200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058300021009     C***                if        prmfir = *blanks
058400021113     C                   eval      cmzfla = 'FIVAB00F'
058500021113     C                   eval      cmzmba = 'FIVAB00F'
058600021009     C***                else
058700021009     C***                eval      cmzfla = prmfir
058800021009     C***                eval      cmzmba = prmfir
058900021009     C***                endif
059000010201     C                   eval      cmznrr = *zeros
059100010201     C                   move      §ctrokvb      cmznrr
059200021018     C                   eval      cmzlba = vlrfl1
059300010201     C                   call(e)   'TIS711C'
059400010201     C                   parm                    dscmz
059500010201     C                   parm      *blanks       esito
059600010201     C                   if        %error
059700010201     C                             or cmzerr = '1'
059800010201     C                             or esito  = '1'
059900010201     C                   eval      wrkesito = '3'
060000010201     C                   endif
060100010205     C                   endif
060200990920     C*
060300000613     C                   ENDSR
060400000613     C***
060500070411
060600070411     C     *pssr         BEGSR
060700070411     C*
060800070411     C                   if        %open(tivin00r)
060900070411     C                   close     tivin00r
061000070411     C                   endif
061100070411     C                   if        %open(fivabwwr)
061200070411     C                   close     fivabwwr
061300070411     C                   endif
061400070411     C                   if        %open(fivatwwr)
061500070411     C                   close     fivatwwr
061600070411     C                   endif
061700070411     C*
061800070411     C* Effettuo la chiamata al CLLE preposto
061900070411     C                   call(e)   'TITVVTC'
062000070411     C                   parm                    parccm
062100070411     C                   parm                    parmbr
062200070411     C                   parm      '2'           paropz
062300070411     C*
062400070411     C                   eval      wrkesito = '2'
062500070411     C*
062600070411     C                   seton                                        LR
062700070411     C*
062800070411     C                   ENDSR     '*CANCL'
062900070411     C***
063000070411
063100990910
063200000613     C     *inzsr        BEGSR
063300990910     C*
063400990910     C     *entry        plist
063500990920     C                   parm                    tivlrds
063600990921     C                   parm      wrkesito      esito
063700000724     C                   parm                    prmlit
063800000710     C                   parm                    prmfir
063900000613     C*
064000000830     C* CALCOLA LA DATA CORRENTE
064100120228     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
064200120228     C                   eval      datcor = %dec(%date() : *ISO)
064300000830     C*
064400000613     C                   ENDSR
