000100090729      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500090729     FFIVABwwr  O    E             DISK    usropn
000600090729     FFIVATwwr  O    E             DISK    usropn
000700000801     D*----------------------------------------------------
000800000801     D* DICHIARAZIOINE VARIABILI DI WRK
000900000801     D*----------------------------------------------------
001000990920     D dscmz         e ds                  inz
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300990920     D tivlrds       e ds                  extname(tivlr00f)
001400090730     D tisi95ds      e ds
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000090729     D depspe          s             16    INZ(*blanks)
002100090729     D curspe          s             16    INZ(*blanks)
002200090804     D contaRec        s              6  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700010213
002800000801     D  Num5_0         s              5  0
002900030107     D  Num7_0         s              7  0
003000000830
003100000830     D*------------------
003200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300000830     D*------------------
003400000830     D WLBDA8          DS                  INZ
003500000830     D  G08DAT                 1      8  0
003600000830     D  G08INV                 9     16  0
003700000830     D  G08ERR                17     17
003800000830     D  G08TGI                18     22  0
003900000830     D*
004000990908
004100010201
004200010201
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600000613     C*
004700050802     C                   EXSR      RWVAB                                        LETT/SCR. VAB/VAT
004800000613     C*
004900010202     C* Effettuo la chiamata al CLLE preposto
005000090729     C                   call(e)   'TITVVTC'
005100010202     C                   parm                    parccm
005200010202     C                   parm                    parmbr
005300010202     C                   parm      '2'           paropz
005400090730     C*
005500090730     C* Effettuo lancio TISI95 solo x chiusura
005600090730     C                   CLEAR                   TISI95DS
005700090730     C                   EVAL      I95TLA = 'C'
005800090730     C                   CALL      'TISI95R'
005900090730     C                   PARM                    TISI95DS
006000000616     C*
006100000801     C
006200010201     C                   seton                                        LR
006300990908
006400000801
006500910830     C*--------------------------------------------------------
006600090729     C* RWVAB     LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR *
006700910830     C*--------------------------------------------------------
006800050802     C     RWVAB         BEGSR
006900990910     C*
007000990914     C                   if        not %open(tivin00r)
007100990908     C                   open      tivin00r
007200990914     C                   endif
007300090729     C                   if        not %open(fivabwwr)
007400090729     C                   open      fivabwwr
007500990914     C                   endif
007600090729     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007700020305     C                   exsr      prevat
007800010201     C*
007900040428     C                   if        chkcall = '0'
008000010202     C*
008100090729     C                   if        not %open(fivatwwr)
008200090729     C                   open      fivatwwr
008300010201     C                   endif
008400990910     C*
008500010201     C                   clear                   §CTROKVB          5 0
008600020305     C                   clear                   §CTROKVT          5 0
008700000801     C                   clear                   §CTRMO            5 0
008800000801     C                   clear                   §CTRNO            5 0
008900090729     C*
009000090729     C                   exsr      INZVAR
009100090729     C                   exsr      DEFCAM
009200990910     C*
009300921023     C                   DO        *HIVAL
009400990913     C*
009500990915     C                   READ      tivin00r                               70
009600000905     C                   if        vindta > *blanks
009700000613     C                   add       1             rrnum
009800000801     C*
009900000801     C                   if        *in70 = *off
010000000801     C                             and
010100000801     C                             (vinflg = *blanks
010200000801     C                              or vinflg = '0'
010300000801     C                              or vinflg = '2')
010400000801     C*
010500000801     C                   clear                   vinmsg
010600090729     C*
010700090729     C                   if        contaRec = *zeros
010800090729     C                   exsr      impvab
010900090729     C                   endif
011000090729     C                   eval      contaRec = contaRec + 1
011100010305     C*
011200090729     C* Salvo il numero speidzione corrente
011300090729     C                   eval      curspe = %subst(vindta:308:16)
011400090729     C*
011500090729     C* Gestisco la rottura di codice x numero speidzione
011600090729     C                   if        curspe = depspe or contaRec = 1
011700090729     C                   else
011800090729     C                   exsr      wrivab                                       => scrivo VAB
011900090729     C                   exsr      INZVAR
012000090729     C                   exsr      DEFCAM
012100090729     C                   exsr      impvab                                       => carico VAB
012200090729     C                   eval      depspe = curspe
012300090729     C                   endif
012400090729     C*
012500090729     C* Scrivo sempre il "chi sono" corrente
012600090729     C                   exsr      wrivat
012700090729     C*
012800010305     C                   endif
012900000905     C*
013000000905     C                   else
013100000905     C                   eval      vinflg = '1'
013200000905     C                   endif
013300000905     C*
013400000905     C  N70              update    tivin000
013500000905     C*
013600991022     C  N70              ENDdo
013700010202     C*
013800010202     C                   endif
013900090729     C*
014000090729     C* Scarico la testata ancora in canna
014100090729     C                   exsr      wrivab                                       => scrivo VAB
014200050802     C*
014300990910     C* Se non ci sono record con errori ...
014400000710     C                   if        §ctrno = 0
014500990910     C* ... restituisco esito OK.
014600990921     C                   eval      wrkesito = '0'
014700990910     C                   else
014800010201     C                   if        §ctrokvb > 0
014900990921     C                   eval      wrkesito = '1'
015000000710     C                   else
015100000710     C                   eval      wrkesito = '2'
015200990910     C                   endif
015300000710     C                   endif
015400990910     C*
015500990914     C                   if        %open(tivin00r)
015600990908     C                   close     tivin00r
015700990914     C                   endif
015800090729     C                   if        %open(fivabwwr)
015900090729     C                   close     fivabwwr
016000990914     C                   endif
016100090729     C                   if        %open(fivatwwr)
016200090729     C                   close     fivatwwr
016300010201     C                   endif
016400021017     C*
016500021017     C                   if        vlrpoi <> 999
016600021017     C                   eval      vlrpoi = vlrpoi
016700021017     C                   endif
016800990910     C*
016900010201     C                   if        §ctrokvb > 0
017000021017     C                             and vlrpoi > *zeros
017100010202     C                   exsr      invio
017200990920     C                   endif
017300990920     C*
017400910830     C                   ENDSR
017500000613     C***
017600010305
017700010305     C*----------------------------------------------------*
017800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
017900010305     C*----------------------------------------------------*
018000020305     C     WRIVAB        BEGSR
018100010305     C*
018200090729     C                   write     fivab000                                     => scarico il VAB
018300010305     C*
018400010305     C                   ENDSR
018500990920
018600000801     C*----------------------------------------------------*
018700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018800000801     C*----------------------------------------------------*
018900010201     C     INZVAR        BEGSR
019000000801     C*
019100010201     C                   Z-ADD     *zeros        Num5_0
019200030107     C                   Z-ADD     *zeros        Num7_0
019300030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
019400030711     C                   MOVEL     '0'           FlgCAS            1
019500090729     C*
019600000801     C                   ENDSR
019700000801     C*----------------------------------------------------*
019800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
019900000801     C*----------------------------------------------------*
020000000801     C     DEFCAM        BEGSR
020100040428     C*
020200090729     C                   CLEAR                   FIVAB000
020300090729     C                   CLEAR                   FIVAB000
020400000801     C*
020500020619     C* Imposto i valori di default...
020600090729     C                   Z-ADD     1201624       VABCCM
020700090729     C                   Z-ADD     1201624       VATCCM
020800021017     C                   Z-ADD     vlrpoi        VABFGS
020900021017     C                   Z-ADD     vlrpoi        VATFGS
021000090729     C                   Z-ADD     120           VABLNP
021100090729     C                   Z-ADD     120           VATLNP
021200050802     C                   Z-ADD     000           VABCTR
021300021017     C                   MOVEL     '7Q'          VABCTM
021400050802     C                   MOVEL     '1'           VABCBO
021500020619     C* ... e poi verifico se sono stati passati come parametri
021600020619     C                   IF        vlrppt > *blanks
021700040428     C                   IF        %subst(vlrppt:1:7) <> *blanks
021800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021900020619     C                   EXSR      CHKNUM
022000020619     C                   IF        PiInt=*on
022100020619     C                   Z-ADD     PiVal         VABCCM
022200020619     C                   Z-ADD     PiVal         VATCCM
022300020619     C                   ENDIF
022400040428     C                   ENDIF
022500040428     C                   IF        %subst(vlrppt:8:3) <> *blanks
022600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022700020619     C                   EXSR      CHKNUM
022800020619     C                   IF        PiInt=*on
022900020619     C                   Z-ADD     PiVal         VABLNP
023000020619     C                   Z-ADD     PiVal         VATLNP
023100020619     C                   ENDIF
023200040428     C                   ENDIF
023300040428     C                   IF        %subst(vlrppt:11:3) <> *blanks
023400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023500020619     C                   EXSR      CHKNUM
023600020619     C                   IF        PiInt=*on
023700020619     C                   Z-ADD     PiVal         VABCTR
023800020619     C                   ENDIF
023900040428     C                   ENDIF
024000020619     C                   ENDIF
024100000801     C*
024200000801     C                   ENDSR
024300000801     C*----------------------------------------------------*
024400090729     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
024500000801     C*----------------------------------------------------*
024600010201     C     IMPVAB        BEGSR
024700010305     C*
024800000801     C                   Z-ADD     *zeros        errore            1 0
024900000830     C                   MOVEL     datcor        VABAAS
025000020305     C                   MOVEL     datcor        VATAAS
025100000830     C                   MOVE      datcor        VABMGS
025200031117     C*
025300050802     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
025400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
025500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
025600020117     C* ==
025700050802     C                   EVAL      VABRD2=%trim(%subst(vindta:88:35))
025800050802     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))
025900050802     C                   EVAL      VABLOD=%trim(%subst(vindta:167:25))
026000050802     C                   EVAL      VABPRD=%trim(%subst(vindta:202:2))
026100050802     C                   EVAL      VABRMA=%trim(%subst(vindta:324:15))
026200050802     C                   EVAL      VABNOT=%trim(%subst(vindta:380:35))
026300050802     C                   EVAL      VABNT2=%trim(%subst(vindta:415:35))
026400090729     C                   EVAL      VABNAS=%trim(%subst(vindta:234:15))
026500090730     C                   EVAL      VABTIC=%trim(%subst(vindta:301:2))
026600051117     C                   IF        %trim(%subst(vindta:47:2)) = '2'
026700051117     C                   EVAL      VABCBO='2'
026800051117     C                   ENDIF
026900030711     C* CAD
027000050802     C                   EVAL      PiStr=%trim(%subst(vindta:158:9))
027100010201     C                   EXSR      CHKNUM
027200010201     C                   IF        PiInt=*on
027300010201     C                   Z-ADD     PiVal         Num5_0
027400021017     C                   MOVEL     Num5_0        VABCAD
027500010201     C                   ELSE
027600010201     C                   ADD       1             errore
027700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
027800021017     C                             + ' ' + 'VABCAD'
027900010201     C                   ENDIF
028000090730     C*
028100090730     C* Se provincia nn valorizzata la reperisco
028200090730     C* tramite TISI95R a seconda dei dati d instradamento presenti
028300090730     C                   IF        VABPRD = *blanks
028400090730     C                   CLEAR                   TISI95DS
028500090730     C                   EVAL      I95TCN = '3'
028600090730     C                   Z-ADD     datcor        I95DAT
028700090730     C                   EVAL      I95NAR = VABNZD
028800090730     C                   EVAL      I95CAP = VABCAD
028900090730     C                   EVAL      I95LOC = VABLOD
029000090730     C                   CALL      'TISI95R'
029100090730     C                   PARM                    TISI95DS
029200090730     C                   EVAL      VABPRD = O95PRV
029300090730     C                   ENDIF
029400120730     C* CCM
029500120730     C                   IF        %subst(vindta:3:7) <> *all'0' AND
029600120730     C                             %subst(vindta:3:7) <> *blanks
029700120730     C                   EVAL      PiStr=%trim(%subst(vindta:3:7))
029800120730     C                   EXSR      CHKNUM
029900120730     C                   IF        PiInt=*on
030000120730     C                   Z-ADD     PiVal         VABCCM
030100120906     C                   Z-ADD     PiVal         VATCCM
030200120730     C                   ELSE
030300120730     C                   ADD       1             errore
030400120730     C                   EVAL      vinmsg = %trimr(vinmsg)
030500120906     C                             + ' ' + 'VABCCM VATCCM'
030600120730     C                   ENDIF
030700120730     C                   ENDIF
030800120730     C* LNP
030900120730     C                   IF        %subst(vindta:11:3) <> *all'0' AND
031000120730     C                             %subst(vindta:11:3) <> *blanks
031100120730     C                   EVAL      PiStr=%trim(%subst(vindta:11:3))
031200120730     C                   EXSR      CHKNUM
031300120730     C                   IF        PiInt=*on
031400120730     C                   Z-ADD     PiVal         VABLNP
031500120906     C                   Z-ADD     PiVal         VATLNP
031600120730     C                   ELSE
031700120730     C                   ADD       1             errore
031800120730     C                   EVAL      vinmsg = %trimr(vinmsg)
031900120906     C                             + ' ' + 'VABLNP VATLNP'
032000120730     C                   ENDIF
032100120730     C                   ENDIF
032200030711     C* RMN
032300090729     C                   EVAL      PiStr=%trim(%subst(vindta:308:16))
032400010201     C                   EXSR      CHKNUM
032500010201     C                   IF        PiInt=*on
032600021017     C                   Z-ADD     PiVal         VABRMN
032700010201     C                   ELSE
032800010201     C                   ADD       1             errore
032900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033000050802     C                             + ' ' + 'VABRMN'
033100010201     C                   ENDIF
033200050802     C* NSP
033300090729     C                   EVAL      PiStr=%trim(%subst(vindta:311:7))
033400050802     C                   EXSR      CHKNUM
033500050802     C                   IF        PiInt=*on
033600050802     C                   Z-ADD     PiVal         Num7_0
033700050802     C                   MOVE(P)   Num7_0        VABNSP
033800050802     C                   MOVE(P)   Num7_0        VATNSP
033900050802     C                   ELSE
034000050802     C                   ADD       1             errore
034100050802     C                   EVAL      vinmsg = %trimr(vinmsg)
034200050802     C                             + ' ' + 'VABNSP VATNSP'
034300050802     C                   ENDIF
034400050802     C* NCL
034500050802     C                   EVAL      PiStr=%trim(%subst(vindta:249:6))
034600030107     C                   EXSR      CHKNUM
034700030107     C                   IF        PiInt=*on
034800040428     C                   ADD       PiVal         VABNCL
034900030107     C                   ELSE
035000030107     C                   ADD       1             errore
035100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
035200030107     C                             + ' ' + 'VABNCL'
035300030107     C                   ENDIF
035400050802     C* PKB
035500050802     C                   EVAL      PiStr=%trim(%subst(vindta:255:9))
035600030107     C                   EXSR      CHKNUM
035700030107     C                   IF        PiNum=*on
035800090928     C***                EVAL      PiVal = PiVal / 1000                         * da grammi a Kg.
035900050802     C                   Z-ADD     PiVal         VABPKB
036000030107     C                   ELSE
036100030107     C                   ADD       1             errore
036200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036300030107     C                             + ' ' + 'VABPKB'
036400030107     C                   ENDIF
036500090730     C* IAS
036600090730     C                   IF        %subst(vindta:216:15) <> *blanks
036700090730     C                   EVAL      PiStr=%trim(%subst(vindta:216:15))
036800090730     C                   EXSR      CHKNUM
036900090730     C                   IF        PiNum=*on
037000090730     C                   Z-ADD     PiVal         VABIAS
037100090730     C                   IF        VABIAS > *zeros
037200090730     C                   EVAL      VABVAS = 'EUR'
037300090730     C                   ENDIF
037400090730     C                   ELSE
037500090730     C                   ADD       1             errore
037600090730     C                   EVAL      vinmsg = %trimr(vinmsg)
037700090730     C                             + ' ' + 'VABIAS'
037800090730     C                   ENDIF
037900090730     C                   ENDIF
038000090730     C* CAS
038100090730     C                   IF        %subst(vindta:286:15) <> *blanks
038200090730     C                   MOVEL     '1'           FlgCAS
038300090730     C                   EVAL      PiStr=%trim(%subst(vindta:286:15))
038400090730     C                   EVAL      VABVCA=%trim(%subst(vindta:303:3))
038500050802     C                   EXSR      CHKNUM
038600050802     C                   IF        PiNum=*on
038700090730     C                   Z-ADD     PiVal         VABCAS
038800050802     C                   ELSE
038900050802     C                   ADD       1             errore
039000050802     C                   EVAL      vinmsg = %trimr(vinmsg)
039100090730     C                             + ' ' + 'VABCAS'
039200050802     C                   ENDIF
039300050802     C                   ENDIF
039400010205     C*
039500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039600051117     C                   IF        FlgCAS <> '0'
039700051117     C                   IF        VABCBO = '1'
039800040428     C                   EVAL      VABCBO = '4'                                 * franco  SI contr.
039900051117     C                   ENDIF
040000051117     C                   IF        VABCBO = '2'
040100051117     C                   EVAL      VABCBO = '6'                                 * franco  SI contr.
040200051117     C                   ENDIF
040300031117     C                   ENDIF
040400020305     C*
040500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040600011113     C                   EXSR      CHKIMPDIV
040700010202     C*
040800000801     C* Ebbene...
040900000801     C                   ADD       1             §CTRMO
041000010201     C                   IF        errore <> *zeros
041100000801     C                   ADD       1             §CTRNO
041200000801     C                   EVAL      vinflg = '2'
041300000801     C                   ELSE
041400010201     C                   ADD       1             §CTROKVB
041500000801     C                   ENDIF
041600000801     C*
041700000801     C                   ENDSR
041800010201     C*----------------------------------------------------*
041900090729     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042000010201     C*----------------------------------------------------*
042100020305     C     WRIVAT        BEGSR
042200090729     C*
042300090729     C                   eval      vatAAS = vabAAS
042400090729     C                   eval      vatLNP = vabLNP
042500090729     C                   eval      vatNRS = vabNRS
042600090729     C                   eval      vatNSP = vabNSP
042700090729     C                   eval      vatTRC = 'E'
042800010201     C*
042900090729     C* Valorizzo il buffer di scrittura del FIVAT
043000090804     C                   eval      vatNOT = %trim(%subst(vindta:28:19))
043100090729     C                   write     FIVAT000
043200040428     C*
043300050802     C                   add       1             §ctrokvt
043400010201     C*
043500010201     C                   ENDSR
043600010202     C*----------------------------------------------------*
043700090729     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043800010202     C*----------------------------------------------------*
043900020305     C     PREVAT        BEGSR
044000010202     C*
044100090729     C* Compongo il nome del membro da dare al FIVATWWR
044200010202     C                   eval      parmbr = vlrhdl
044300010202     C                   movel     'M'           parmbr
044400050802     C                   eval      parccm = '00051381'
044500010202     C                   eval      paropz = '1'
044600010202     C* Effettuo la chiamata al CLLE preposto
044700090729     C                   call(e)   'TITVVTC'
044800010202     C                   parm                    parccm
044900010202     C                   parm                    parmbr
045000010202     C                   parm                    paropz
045100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045200010202     C                   if        %error
045300010202     C                   movel     '1'           chkcall
045400010202     C                   else
045500010202     C                   movel     '0'           chkcall
045600010202     C                   endif
045700010202     C*
045800010202     C                   ENDSR
045900000801     C*----------------------------------------------------*
046000000801     C*  CONTROLLO NUMERICITA' CAMPI
046100000801     C*----------------------------------------------------*
046200000801     C     CHKNUM        BEGSR
046300000801     C*
046400000801     C                   call(e)   'ISNUMERIC'
046500000801     C                   PARM                    PiStr            30
046600090918     C                   PARM      '.'           PiDecChr          1
046700000801     C                   PARM      *ZEROS        PiVal            30 9
046800000801     C                   PARM      '0'           PiInt             1
046900000801     C                   PARM      '0'           PiNum             1
047000000801     C                   IF        %error
047100000801     C                   EVAL      PiInt=*off
047200030107     C                   EVAL      PiNum=*off
047300000801     C                   ENDIF
047400000801     C*
047500000801     C                   ENDSR
047600000801     C***
047700000801
047800011113
047900011113     C*----------------------------------------------------*
048000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048100011113     C*----------------------------------------------------*
048200011113     C     CHKIMPDIV     BEGSR
048300011113     C*
048400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048500011113     C                   Z-ADD     *zeros        wrkDec            9 9
048600011113     C*
048700011113     C* Come prima cosa effettuo considerazioni sulla divisa
048800011113     C                   IF        vabIAS > *zeros
048900011113     C                   IF        vabVAS <> 'EUR'
049000011113     C                   EVAL      vabVAS =  'ITL'
049100011113     C                   ENDIF
049200011113     C                   ENDIF
049300011113     C*
049400011113     C                   IF        vabCAS > *zeros
049500011113     C                   IF        vabVCA <> 'EUR'
049600011113     C                   EVAL      vabVCA =  'ITL'
049700011113     C                   ENDIF
049800011113     C                   ENDIF
049900011113     C*
050000011113     C                   IF        vabVMD > *zeros
050100020305     C                   IF        vabVAD <> 'EUR'
050200011113     C                   EVAL      vabVAD =  'ITL'
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050700011113     C                   Z-ADD     vabIAS        wrkDec
050800011113     C                   IF        wrkDec > *zeros
050900011113     C                   IF        vabVAS = 'ITL'
051000011113     C                   EVAL      vabIAS = *zeros
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051500011113     C                   Z-ADD     vabCAS        wrkDec
051600011113     C                   IF        wrkDec > *zeros
051700011113     C                   IF        vabVCA = 'ITL'
051800011113     C                   EVAL      vabCAS = *zeros
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052300011113     C                   Z-ADD     vabVMD        wrkDec
052400011113     C                   IF        wrkDec > *zeros
052500011113     C                   IF        vabVAD = 'ITL'
052600011113     C                   EVAL      vabVMD = *zeros
052700011113     C                   ENDIF
052800011113     C                   ENDIF
052900011113     C*
053000011113     C                   ENDSR
053100011113     C***
053200021017
053300000801
053400000801
053500990920      /TITLE Invio dei dati al punto operativo.
053600010202     C     invio         BEGSR
053700990920     C*
053800090729     C* 1° invio FIVAT
053900010201     C                   reset                   dscmz
054000021017     C                   move      vlrpoi        cmzdst
054100090729     C                   eval      cmzfld = 'FIVATWWR'
054200010201     C                   eval      cmzmbd = vlrhdl
054300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054400021009     C***                if        prmfir = *blanks
054500090729     C                   eval      cmzfla = 'FIVAT00F'
054600090729     C                   eval      cmzmba = 'FIVAT00F'
054700021009     C***                else
054800021009     C***                eval      cmzfla = prmfir
054900021009     C***                eval      cmzmba = prmfir
055000021009     C***                endif
055100010201     C                   eval      cmznrr = *zeros
055200020305     C                   move      §ctrokvt      cmznrr
055300021018     C                   eval      cmzlba = vlrfl1
055400010201     C                   call(e)   'TIS711C'
055500010201     C                   parm                    dscmz
055600010201     C                   parm      *blanks       esito
055700010205     C                   if        %error
055800010205     C                             or cmzerr = '1'
055900010205     C                             or esito  = '1'
056000010205     C                   eval      wrkesito = '3'
056100010205     C                   else
056200010201     C*
056300090729     C* 2° invio FIVAB
056400010201     C                   reset                   dscmz
056500021017     C                   move      vlrpoi        cmzdst
056600010201     C                   eval      cmzfld = vlrfou
056700010201     C                   eval      cmzmbd = vlrhdl
056800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056900021009     C***                if        prmfir = *blanks
057000090729     C                   eval      cmzfla = 'FIVAB00F'
057100090729     C                   eval      cmzmba = 'FIVAB00F'
057200021009     C***                else
057300021009     C***                eval      cmzfla = prmfir
057400021009     C***                eval      cmzmba = prmfir
057500021009     C***                endif
057600010201     C                   eval      cmznrr = *zeros
057700010201     C                   move      §ctrokvb      cmznrr
057800021018     C                   eval      cmzlba = vlrfl1
057900010201     C                   call(e)   'TIS711C'
058000010201     C                   parm                    dscmz
058100010201     C                   parm      *blanks       esito
058200010201     C                   if        %error
058300010201     C                             or cmzerr = '1'
058400010201     C                             or esito  = '1'
058500010201     C                   eval      wrkesito = '3'
058600010201     C                   endif
058700010205     C                   endif
058800990920     C*
058900000613     C                   ENDSR
059000000613     C***
059100070502
059200070502
059300070502
059400070502     C     *pssr         BEGSR
059500070502     C*
059600090729     C                   if        %open(fivabwwr)
059700090729     C                   close     fivabwwr
059800070502     C                   endif
059900090729     C                   if        %open(fivatwwr)
060000090729     C                   close     fivatwwr
060100070502     C                   endif
060200070502     C*
060300070502     C* Effettuo la chiamata al CLLE preposto
060400090729     C                   call(e)   'TITVVTC'
060500070502     C                   parm                    parccm
060600070502     C                   parm                    parmbr
060700070502     C                   parm      '2'           paropz
060800070502     C*
060900070502     C                   eval      wrkesito = '2'
061000070502     C*
061100070502     C                   seton                                        LR
061200070502     C*
061300070502     C                   ENDSR     '*CANCL'
061400070502     C***
061500070502
061600070502
061700990910
061800000613     C     *inzsr        BEGSR
061900990910     C*
062000990910     C     *entry        plist
062100990920     C                   parm                    tivlrds
062200990921     C                   parm      wrkesito      esito
062300000724     C                   parm                    prmlit
062400000710     C                   parm                    prmfir
062500000613     C*
062600000830     C* CALCOLA LA DATA CORRENTE
062700000830     C                   time                    wn14             14 0
062800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062900050802     C                   movel     wn14          oracor            6 0          *ORA
063000000830     C                   z-add     wn8           g08dat
063100000830     C                   z-add     *zeros        g08inv
063200000830     C                   movel     '0'           g08err
063300000830     C                   call      'XSRDA8'
063400000830     C                   parm                    wlbda8
063500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063600000830     C*
063700000613     C                   ENDSR
063800000613     C***
