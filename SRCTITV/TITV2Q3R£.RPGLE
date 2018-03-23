000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
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
001600130606     D fivabds       e ds                  extname(fivab00f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600000830
002700041025     D*------------------
002800041025     D* DS REPERIMENTO NUMERATORE
002900041025     D*------------------
003000041025     D trul33ds      e ds                  inz
003100041025     D*------------------
003200041025     D* DS ARCHITETTURA
003300041025     D*------------------
003400041025     D kpjba         e ds                  inz
003500041025     D*------------------
003600990908
003700010201
003800010201
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200000613     C*
004300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
004400070719     C*
004500070719     C* Esegue lancio TISI95R solo x chiusura
004600070719     C                   CLEAR                   TISI95DS
004700070719     C                   EVAL      I95TLA = 'C'
004800070719     C                   CALL      'TISI95R'
004900070719     C                   PARM                    TISI95DS
005000000613     C*
005100010202     C* Effettuo la chiamata al CLLE preposto
005200040506     C                   call(e)   'TITVVTC'
005300010202     C                   parm                    parccm
005400010202     C                   parm                    parmbr
005500010202     C                   parm      '2'           paropz
005600000616     C*
005700010201     C                   seton                                        LR
005800990908
005900000801
006000910830     C*--------------------------------------------------------
006100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006200910830     C*--------------------------------------------------------
006300040526     C     RWFILE        BEGSR
006400990910     C*
006500990914     C                   if        not %open(tivin00r)
006600990908     C                   open      tivin00r
006700990914     C                   endif
006800021113     C                   if        not %open(fivabwwr)
006900021113     C                   open      fivabwwr
007000990914     C                   endif
007100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007200020305     C                   exsr      prevat
007300010201     C*
007400010202     C                   if        chkcall = '0'
007500010202     C*
007600021113     C                   if        not %open(fivatwwr)
007700021113     C                   open      fivatwwr
007800010201     C                   endif
007900990910     C*
008000010201     C                   clear                   §CTROKVB          5 0
008100020305     C                   clear                   §CTROKVT          5 0
008200000801     C                   clear                   §CTRMO            5 0
008300000801     C                   clear                   §CTRNO            5 0
008400130606     C*
008500130606     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
008600130606     C                   if        *in53
008700130606     C                   call      'TIS7P2SR'
008800130606     C                   parm      'O'           pIn_Opz           1
008900130606     C                   parm                    tivlrds
009000130606     C                   parm                    FIVABDS
009100130606     C                   parm      *blanks       pIn_CdIdAz        2
009200130606     C                   parm      *blanks       pIn_MaskPDF      50
009300130606     C                   parm      *blanks       pOut_Esito        1
009400130606     C                   endif
009500040910     C*
009600921023     C                   DO        *HIVAL
009700990913     C*
009800990915     C                   READ      tivin00r                               70
009900040910     C                   if        vindta > *blanks
010000000613     C                   add       1             rrnum
010100000801     C*
010200000801     C                   if        *in70 = *off
010300000801     C                             and
010400000801     C                             (vinflg = *blanks
010500000801     C                              or vinflg = '0'
010600000801     C                              or vinflg = '2')
010700000801     C*
010800000801     C                   clear                   vinmsg
010900000801     C                   eval      vinflg = '1'
011000040910     C*
011100040910     C* Eseguo routine d traduzione
011200040910     C                   exsr      impvabvat
011300040802     C*
011400010305     C                   endif
011500000905     C*
011600000905     C                   else
011700000905     C                   eval      vinflg = '1'
011800000905     C                   endif
011900000905     C*
012000000905     C  N70              update    tivin000
012100000905     C*
012200991022     C  N70              ENDdo
012300130606     C*
012400130606     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
012500130606     C                   if        *in53
012600130606     C                   call      'TIS7P2SR'
012700130606     C                   parm      'C'           pIn_Opz           1
012800130606     C                   parm                    tivlrds
012900130606     C                   parm                    FIVABDS
013000130606     C                   parm      *blanks       pIn_CdIdAz        2
013100130606     C                   parm      *blanks       pIn_MaskPDF      50
013200130606     C                   parm      *blanks       pOut_Esito        1
013300130606     C                   endif
013400010202     C*
013500010202     C                   endif
013600130606     C*
013700990910     C* Se non ci sono record con errori ...
013800000710     C                   if        §ctrno = 0
013900990910     C* ... restituisco esito OK.
014000990921     C                   eval      wrkesito = '0'
014100990910     C                   else
014200010201     C                   if        §ctrokvb > 0
014300990921     C                   eval      wrkesito = '1'
014400000710     C                   else
014500000710     C                   eval      wrkesito = '2'
014600990910     C                   endif
014700000710     C                   endif
014800990910     C*
014900990914     C                   if        %open(tivin00r)
015000990908     C                   close     tivin00r
015100990914     C                   endif
015200021113     C                   if        %open(fivabwwr)
015300021113     C                   close     fivabwwr
015400990914     C                   endif
015500021113     C                   if        %open(fivatwwr)
015600021113     C                   close     fivatwwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   if        §ctrokvb > 0
016000000724     C                             and vlrpoi <> *zeros
016100010202     C                   exsr      invio
016200990920     C                   endif
016300990920     C*
016400910830     C                   ENDSR
016500000613     C***
016600990920
016700000801     C*----------------------------------------------------*
016800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016900000801     C*----------------------------------------------------*
017000010201     C     INZVAR        BEGSR
017100000801     C*
017200040802     C                   Z-ADD     *zeros        Num5_0            5 0
017300040802     C                   MOVEL     '0'           FlgCAS            1
017400000801     C*
017500000801     C                   ENDSR
017600000801     C*----------------------------------------------------*
017700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017800000801     C*----------------------------------------------------*
017900000801     C     DEFCAM        BEGSR
018000000801     C*
018100021113     C                   CLEAR                   FIVAB000
018200040802     C                   CLEAR                   FIVAT000
018300020619     C* Imposto i valori di default...
018400130606     C                   Z-ADD     0203301       VABCCM
018500130606     C                   Z-ADD     0203301       VATCCM
018600130606     C                   Z-ADD     020           VABLNP
018700130606     C                   Z-ADD     020           VATLNP
018800070531     C                   Z-ADD     000           VABCTR
018900070531     C                   MOVEL     '7Q'          VABCTM
019000040823     C                   MOVEL     '1'           VABCBO
019100020619     C* ... e poi verifico se sono stati passati come parametri
019200020619     C                   IF        vlrppt > *blanks
019300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019500020619     C                   EXSR      CHKNUM
019600020619     C                   IF        PiInt=*on
019700020619     C                   Z-ADD     PiVal         VABCCM
019800020619     C                   Z-ADD     PiVal         VATCCM
019900020619     C                   ENDIF
020000040506     C                   ENDIF
020100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020300020619     C                   EXSR      CHKNUM
020400020619     C                   IF        PiInt=*on
020500020619     C                   Z-ADD     PiVal         VABLNP
020600020619     C                   Z-ADD     PiVal         VATLNP
020700040506     C                   ENDIF
020800020619     C                   ENDIF
020900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021100020619     C                   EXSR      CHKNUM
021200020619     C                   IF        PiInt=*on
021300020619     C                   Z-ADD     PiVal         VABCTR
021400040506     C                   ENDIF
021500020619     C                   ENDIF
021600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
021700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
021800060202     C                   ENDIF
021900130606     C*
022000130606     C* Verifico se sui parametri del traduttore è richiesto la scrittura forzata del
022100130606     C* tipo record 'P' del VAT (packing list PDF)
022200130606     C                   if        %subst(vlrppt:16:1) = 'S'
022300130606     C                   seton                                        53
022400130606     C                   else
022500130606     C                   setoff                                       53
022600130606     C                   endif
022700130606     C*
022800020619     C                   ENDIF
022900000801     C*
023000000801     C                   ENDSR
023100000801     C*----------------------------------------------------*
023200040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
023300000801     C*----------------------------------------------------*
023400040910     C     IMPVABVAT     BEGSR
023500040910     C*
023600040910     C* Traduzione relativa ai tipi record del file del cliente
023700040910     C*
023800040910     C***
023900130606     C* ...tipo record 'ECE' (testata - partner ID)
024000130606     C***                IF        %subst(vindta:1:6) = 'ECESTD'
024100071210     C* PartnerID (VABCCM)
024200130606     C***                MOVEL     *blanks       wPtnrID          10
024300130606     C***                EVAL      wPtnrID=%trim(%subst(vindta:280:10))
024400071210     C*
024500130606     C***                ENDIF
024600071210     C*
024700071210     C***
024800071210     C* ...tipo record 'EBE' (testata)
024900130606     C                   IF        %subst(vindta:1:6) = 'EBESTD'
025000071210     C* Resetto indicatore d anomalia sul singolo record
025100071210     C                   eval      vinflg = '1'
025200071210     C* ......inizializzazioni iniziali e formati record file Bartolini
025300071210     C                   EXSR      INZVAR
025400071210     C                   EXSR      DEFCAM
025500071210     C*
025600071210     C                   Z-ADD     *zeros        errore            1 0
025700071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
025800071210     C                   MOVEL     datcor        VABAAS
025900071210     C                   MOVEL     datcor        VATAAS
026000071210     C                   MOVE      datcor        VABMGS
026100071210     C                   MOVE(P)   vlrpoi        VABFGS
026200071210     C                   MOVE(P)   vlrpoi        VATFGS
026300071210     C* ......VABNSP/VATNSP
026400071210     C* NSP => Stacco un numeratore da AZNUM
026500071210     C                   clear                   TRUL33DS
026600071210     C                   eval      I33OPE = *zeros
026700071210     C                   eval      I33CNU = 302
026800071210     C                   eval      I33NUM = 1
026900071210     C                   movel     TRUL33DS      KPJBU
027000071210     C                   call      'TRUL33R'
027100071210     C                   parm                    KPJBA
027200071210     C                   movel     KPJBU         TRUL33DS
027300071210     C                   if        O33ERR = *zeros
027400071210     C                   z-add     O33NRF        VABNSP
027500071210     C                   z-add     O33NRF        VATNSP
027600071210     C                   else
027700071210     C                   Z-ADD     1             errore
027800071210     C                   EVAL      vinmsg = %trimr(vinmsg)
027900071210     C                             + ' ' + 'VABNSP VATNSP'
028000071210     C                   endif
028100071210     C* ......VABNOT
028200130606     C**                 EVAL      VABNOT='ORD CLI: ' +
028300130606     C***                                 %trim(%subst(vindta:393:25))
028400080211     C* ......VABNT2
028500130606     C***                EVAL      VABNT2='CUST ACCOUNT #: ' +
028600130606     C***                                 %trim(%subst(vindta:9:13))
028700060202     C* ......VABRMN
028800130606     C                   EVAL      PiStr=%trim(%subst(vindta:25:15))
028900060202     C                   EXSR      CHKNUM
029000060202     C                   IF        PiInt=*on
029100060202     C                   Z-ADD     PiVal         VABRMN
029200060202     C                   ELSE
029300060202     C                   ADD       1             errore
029400060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029500060202     C                             + ' ' + 'VABRMN'
029600060202     C                   ENDIF
029700130606     C* ......VABCAS
029800130606     C                   IF        %trim(%subst(vindta:220:11)) <> *blanks AND
029900130606     C                             %trim(%subst(vindta:220:11)) <> *zeros
030000130606     C                   MOVEL     '1'           FlgCAS
030100130606     C                   EVAL      VABVCA = 'EUR'
030200130606     C                   EVAL      PiStr=%trim(%subst(vindta:220:11))
030300130606     C                   EXSR      CHKNUM
030400130606     C                   IF        PiNum=*on
030500130606     C                   Z-ADD(H)  PiVal         VABCAS
030600130606     C                   ELSE
030700130606     C                   ADD       1             errore
030800130606     C                   EVAL      vinmsg = %trimr(vinmsg)
030900130606     C                             + ' ' + 'VABCAS'
031000130606     C                   ENDIF
031100130606     C                   ENDIF
031200060202     C* ......VABPKB
031300130606     C                   EVAL      PiStr=%trim(%subst(vindta:318:9))
031400060202     C                   EXSR      CHKNUM
031500060202     C                   IF        PiNum=*on
031600070726     C                   EVAL      PiVal = PiVal / 1000                         da grammi a kg
031700070726     C                   Z-ADD(H)  PiVal         VABPKB
031800060202     C                   ELSE
031900060202     C                   ADD       1             errore
032000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
032100060202     C                             + ' ' + 'VABPKB'
032200060202     C                   ENDIF
032300071210     C* ......VABVLB
032400130606     C                   EVAL      PiStr=%trim(%subst(vindta:327:9))
032500130606     C                   EXSR      CHKNUM
032600130606     C                   IF        PiNum=*on
032700130606     C                   EVAL      PiVal = PiVal / 1000                         da dm3 a m3
032800130606     C                   Z-ADD(H)  PiVal         VABVLB
032900130606     C                   ELSE
033000130606     C                   ADD       1             errore
033100130606     C                   EVAL      vinmsg = %trimr(vinmsg)
033200130606     C                             + ' ' + 'VABVLB'
033300130606     C                   ENDIF
033400071210     C* ......VABRMA
033500130606     C                   EVAL      VABRMA=%trim(%subst(vindta:12:13))
033600060202     C* ......VABRSD
033700130606     C                   EVAL      VABRSD=%trim(%subst(vindta:56:35))
033800060202     C* ......VABIND
033900130606     C                   EVAL      VABIND=%trim(%subst(vindta:91:35))
034000130606     C* ......VABPRD
034100130606     C                   EVAL      VABLOD=%trim(%subst(vindta:196:35))
034200060202     C* ......VABLOD
034300130606     C                   EVAL      VABLOD=%trim(%subst(vindta:240:35))
034400060202     C* ......VABCAD
034500130606     C                   EVAL      PiStr=%trim(%subst(vindta:231:9))
034600060202     C                   EXSR      CHKNUM
034700060202     C                   IF        PiInt=*on
034800060202     C                   Z-ADD     PiVal         Num5_0
034900060202     C                   MOVEL(P)  Num5_0        VABCAD
035000060202     C                   ELSE
035100060202     C                   ADD       1             errore
035200060202     C                   EVAL      vinmsg = %trimr(vinmsg)
035300060202     C                             + ' ' + 'VABCAD'
035400060202     C                   ENDIF
035500060202     C* ......VABPRD
035600070719     C* Reperisco la provincia dal CAP e dalla località
035700070719     C                   IF        VABPRD  = *blanks AND
035800070719     C                             VABCAD <> *blanks
035900070719     C                   CLEAR                   TISI95DS
036000070719     C                   EVAL      I95TCN = '3'
036100070719     C                   Z-ADD     datcor        I95DAT
036200070719     C                   EVAL      I95CAP = VABCAD
036300070719     C                   EVAL      I95LOC = VABLOD
036400070719     C                   CALL      'TISI95R'
036500070719     C                   PARM                    TISI95DS
036600070719     C                   EVAL      VABPRD = O95PRV
036700070719     C                   ENDIF
036800130606     C*
036900071210     C* ......VABNCL
037000130606     C                   EVAL      PiStr=%trim(%subst(vindta:313:5))
037100071210     C                   EXSR      CHKNUM
037200071210     C                   IF        PiInt=*on
037300071210     C                   Z-ADD     PiVal         VABNCL
037400071210     C                   ELSE
037500071210     C                   ADD       1             errore
037600071210     C                   EVAL      vinmsg = %trimr(vinmsg)
037700071210     C                             + ' ' + 'VABNCL'
037800071210     C                   ENDIF
037900060202     C*
038000060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038100060202     C                   IF        FlgCAS <> '0'
038200060202     C                   IF        VABCBO = '1'
038300060202     C                   EVAL      VABCBO = '4'
038400060202     C                   ELSE
038500060202     C                   EVAL      VABCBO = '6'
038600060202     C                   ENDIF
038700060202     C                   ENDIF
038800060202     C*
038900060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039000060202     C                   EXSR      CHKIMPDIV
039100130606     C*
039200130606     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
039300130606     C                   if        *in53
039400130606     C                   eval      pIn_MaskPDF = 'BL_&0RMN*_&RMA*.pdf'
039500130606     C                   call      'TIS7P2SR'
039600130606     C                   parm      'W'           pIn_Opz           1
039700130606     C                   parm                    tivlrds
039800130606     C                   parm                    FIVABDS
039900130606     C                   parm      '10'          pIn_CdIdAz        2
040000130606     C                   parm                    pIn_MaskPDF      50
040100130606     C                   parm      *blanks       pOut_Esito        1
040200130606     C                   endif
040300130606     C*
040400060202     C* ...e in fine...
040500060202     C                   WRITE     FIVAB000
040600040910     C*
040700040910     C                   ENDIF
040800040910     C***
040900070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
041000130923     C***                IF        %subst(vindta:1:6) = 'EBCSTD'
041100070719     C* Resetto indicatore d anomalia sul singolo record
041200130923     C***                eval      vinflg = '1'
041300040910     C* ......VATNOT
041400130923     C***                EVAL      VATNOT=%trim(%subst(vindta:40:18))
041500130923     C***                EVAL      VATTRC = 'E'
041600070719     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
041700130923     C***                IF        VATNOT <> *blanks
041800130923     C***                WRITE     FIVAT000
041900130923     C***                ENDIF
042000040910     C*
042100130923     C***                ENDIF
042200130923     C*
042300130923     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
042400130923     C                   IF        %subst(vindta:1:6) = 'EBCSPC'
042500130923     C* Resetto indicatore d anomalia sul singolo record
042600130923     C                   eval      vinflg = '1'
042700130923     C* ......VATNOT
042800130923     C                   EVAL      VATNOT=%trim(%subst(vindta:65:12))
042900130923     C                   EVAL      VATTRC = 'E'
043000130923     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
043100130923     C                   IF        VATNOT <> *blanks
043200130923     C                   WRITE     FIVAT000
043300130923     C                   ENDIF
043400130923     C*
043500130923     C                   ENDIF
043600010202     C*
043700000801     C* Ebbene...
043800000801     C                   ADD       1             §CTRMO
043900010201     C                   IF        errore <> *zeros
044000000801     C                   ADD       1             §CTRNO
044100000801     C                   EVAL      vinflg = '2'
044200000801     C                   ELSE
044300010201     C                   ADD       1             §CTROKVB
044400000801     C                   ENDIF
044500000801     C*
044600000801     C                   ENDSR
044700010202     C*----------------------------------------------------*
044800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044900010202     C*----------------------------------------------------*
045000020305     C     PREVAT        BEGSR
045100010202     C*
045200021113     C* Compongo il nome del membro da dare al FIVATWWR
045300010202     C                   eval      parmbr = vlrhdl
045400010202     C                   movel     'M'           parmbr
045500060113     C                   eval      parccm = vlrksc
045600010202     C                   eval      paropz = '1'
045700010202     C* Effettuo la chiamata al CLLE preposto
045800040506     C                   call(e)   'TITVVTC'
045900010202     C                   parm                    parccm
046000010202     C                   parm                    parmbr
046100010202     C                   parm                    paropz
046200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046300010202     C                   if        %error
046400010202     C                   movel     '1'           chkcall
046500010202     C                   else
046600010202     C                   movel     '0'           chkcall
046700010202     C                   endif
046800010202     C*
046900010202     C                   ENDSR
047000000801     C*----------------------------------------------------*
047100000801     C*  CONTROLLO NUMERICITA' CAMPI
047200000801     C*----------------------------------------------------*
047300000801     C     CHKNUM        BEGSR
047400000801     C*
047500000801     C                   call(e)   'ISNUMERIC'
047600000801     C                   PARM                    PiStr            30
047700130606     C                   PARM      '.'           PiDecChr          1
047800000801     C                   PARM      *ZEROS        PiVal            30 9
047900000801     C                   PARM      '0'           PiInt             1
048000000801     C                   PARM      '0'           PiNum             1
048100000801     C                   IF        %error
048200000801     C                   EVAL      PiInt=*off
048300000801     C                   ENDIF
048400000801     C*
048500000801     C                   ENDSR
048600000801     C***
048700000801
048800011113
048900011113     C*----------------------------------------------------*
049000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049100011113     C*----------------------------------------------------*
049200011113     C     CHKIMPDIV     BEGSR
049300011113     C*
049400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049500011113     C                   Z-ADD     *zeros        wrkDec            9 9
049600011113     C*
049700011113     C* Come prima cosa effettuo considerazioni sulla divisa
049800011113     C                   IF        vabIAS > *zeros
049900011113     C                   IF        vabVAS <> 'EUR'
050000011113     C                   EVAL      vabVAS =  'ITL'
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C                   IF        vabCAS > *zeros
050500011113     C                   IF        vabVCA <> 'EUR'
050600011113     C                   EVAL      vabVCA =  'ITL'
050700011113     C                   ENDIF
050800011113     C                   ENDIF
050900011113     C*
051000011113     C                   IF        vabVMD > *zeros
051100020305     C                   IF        vabVAD <> 'EUR'
051200011113     C                   EVAL      vabVAD =  'ITL'
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051700011113     C                   Z-ADD     vabIAS        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVAS = 'ITL'
052000011113     C                   EVAL      vabIAS = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052500011113     C                   Z-ADD     vabCAS        wrkDec
052600011113     C                   IF        wrkDec > *zeros
052700011113     C                   IF        vabVCA = 'ITL'
052800011113     C                   EVAL      vabCAS = *zeros
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053300011113     C                   Z-ADD     vabVMD        wrkDec
053400011113     C                   IF        wrkDec > *zeros
053500011113     C                   IF        vabVAD = 'ITL'
053600011113     C                   EVAL      vabVMD = *zeros
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C                   ENDSR
054100011113     C***
054200011113
054300011113
054400000801
054500000801
054600990920      /TITLE Invio dei dati al punto operativo.
054700010202     C     invio         BEGSR
054800990920     C*
054900021113     C* 1° invio FIVAT
055000010201     C                   reset                   dscmz
055100010201     C                   move      vlrpoi        cmzdst
055200021113     C                   eval      cmzfld = 'FIVATWWR'
055300010201     C                   eval      cmzmbd = vlrhdl
055400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055500021009     C***                if        prmfir = *blanks
055600021113     C                   eval      cmzfla = 'FIVAT00F'
055700021113     C                   eval      cmzmba = 'FIVAT00F'
055800021009     C***                else
055900021009     C***                eval      cmzfla = prmfir
056000021009     C***                eval      cmzmba = prmfir
056100021009     C***                endif
056200010201     C                   eval      cmznrr = *zeros
056300020305     C                   move      §ctrokvt      cmznrr
056400021018     C                   eval      cmzlba = vlrfl1
056500010201     C                   call(e)   'TIS711C'
056600010201     C                   parm                    dscmz
056700010201     C                   parm      *blanks       esito
056800010205     C                   if        %error
056900010205     C                             or cmzerr = '1'
057000010205     C                             or esito  = '1'
057100010205     C                   eval      wrkesito = '3'
057200010205     C                   else
057300010201     C*
057400021113     C* 2° invio FIVAB
057500010201     C                   reset                   dscmz
057600010201     C                   move      vlrpoi        cmzdst
057700010201     C                   eval      cmzfld = vlrfou
057800010201     C                   eval      cmzmbd = vlrhdl
057900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058000021009     C***                if        prmfir = *blanks
058100021113     C                   eval      cmzfla = 'FIVAB00F'
058200021113     C                   eval      cmzmba = 'FIVAB00F'
058300021009     C***                else
058400021009     C***                eval      cmzfla = prmfir
058500021009     C***                eval      cmzmba = prmfir
058600021009     C***                endif
058700010201     C                   eval      cmznrr = *zeros
058800010201     C                   move      §ctrokvb      cmznrr
058900021018     C                   eval      cmzlba = vlrfl1
059000010201     C                   call(e)   'TIS711C'
059100010201     C                   parm                    dscmz
059200010201     C                   parm      *blanks       esito
059300010201     C                   if        %error
059400010201     C                             or cmzerr = '1'
059500010201     C                             or esito  = '1'
059600010201     C                   eval      wrkesito = '3'
059700010201     C                   endif
059800010205     C                   endif
059900990920     C*
060000000613     C                   ENDSR
060100000613     C***
060200070411
060300070411     C     *pssr         BEGSR
060400070411     C*
060500070411     C                   if        %open(tivin00r)
060600070411     C                   close     tivin00r
060700070411     C                   endif
060800070411     C                   if        %open(fivabwwr)
060900070411     C                   close     fivabwwr
061000070411     C                   endif
061100070411     C                   if        %open(fivatwwr)
061200070411     C                   close     fivatwwr
061300070411     C                   endif
061400070411     C*
061500070411     C* Effettuo la chiamata al CLLE preposto
061600070411     C                   call(e)   'TITVVTC'
061700070411     C                   parm                    parccm
061800070411     C                   parm                    parmbr
061900070411     C                   parm      '2'           paropz
062000070411     C*
062100070411     C                   eval      wrkesito = '2'
062200070411     C*
062300070411     C                   seton                                        LR
062400070411     C*
062500070411     C                   ENDSR     '*CANCL'
062600070411     C***
062700070411
062800990910
062900000613     C     *inzsr        BEGSR
063000990910     C*
063100990910     C     *entry        plist
063200990920     C                   parm                    tivlrds
063300990921     C                   parm      wrkesito      esito
063400000724     C                   parm                    prmlit
063500000710     C                   parm                    prmfir
063600000613     C*
063700000830     C* CALCOLA LA DATA CORRENTE
063800130606     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063900130606     C                   eval      datcor = %dec(%date() :*ISO)
064000000830     C*
064100000613     C                   ENDSR
064200000613     C***
