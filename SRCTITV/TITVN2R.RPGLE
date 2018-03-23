000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050627     Ftabel00f  if   e           k disk
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800990908
000900050627     D*------------
001000050627     D* SCHIERE
001100050627     D*------------
001200050627     D skNAZISO        S              3    DIM(1000)
001300050627     D skNAZBAR        S              3    DIM(1000)
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100040506     D tisi95ds      e ds
002200050627     D ds15          e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200050627     D depspe          s             15    INZ(*blanks)
003300050627     D curspe          s             15    INZ(*blanks)
003400050628     D cntspe          s              7  0 INZ(*zeros)
003500050627     D jNAZ            s              5  0 INZ(*zeros)
003600050628     D wpkb            s              7  2
003700000830
003800000830     D*------------------
003900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004000000830     D*------------------
004100000830     D WLBDA8          DS                  INZ
004200000830     D  G08DAT                 1      8  0
004300000830     D  G08INV                 9     16  0
004400000830     D  G08ERR                17     17
004500000830     D  G08TGI                18     22  0
004600050627     D*------------------
004700050627     D* DS REPERIMENTO NUMERATORE
004800050627     D*------------------
004900050928     D**!!! trul33ds      e ds                  inz
005000050627     D*------------------
005100050627     D* DS ARCHITETTURA
005200050627     D*------------------
005300050928     D**!!! kpjba         e ds                  inz
005400050627     D*------------------
005500000830     D*
005600990908
005700010201
005800010201
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000613     C*
006300050627     C                   EXSR      CARTAB                                       CARICA TABELLE
006400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006500000613     C*
006600010202     C* Effettuo la chiamata al CLLE preposto
006700040506     C                   call(e)   'TITVVTC'
006800010202     C                   parm                    parccm
006900010202     C                   parm                    parmbr
007000010202     C                   parm      '2'           paropz
007100050201     C*
007200050201     C* Effettuo lancio TISI95 solo x chiusura
007300050201     C                   CLEAR                   TISI95DS
007400050201     C                   EVAL      I95TLA = 'C'
007500050201     C                   CALL      'TISI95R'
007600050201     C                   PARM                    TISI95DS
007700000616     C*
007800000801     C
007900010201     C                   seton                                        LR
008000050627
008100050627     C*--------------------------------------------------------
008200050627     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
008300050627     C*--------------------------------------------------------
008400050627     C     CARTAB        BEGSR
008500050627     C*
008600050627     C* TABELLA '15' - NAZIONI
008700050627     C                   clear                   skNAZISO
008800050627     C                   clear                   skNAZBAR
008900050627     C                   eval      tblKUT = 1
009000050627     C                   eval      tblCOD = '15'
009100050627     C     KEYtabP       setll     tabel00f
009200050627     C     KEYtabP       reade     tabel00f
009300050627     C                   dow       not %eof(tabel00f)
009400050627     C                   if        tblFLG = *blanks
009500050627     C                   movel(p)  tblUNI        ds15
009600050627     C                   add       1             jNAZ
009700050627     C                   eval      skNAZBAR(jNAZ) = tblKEY
009800050627     C                   eval      skNAZISO(jNAZ) = §15COD
009900050627     C                   endif
010000050627     C     KEYtabP       reade     tabel00f
010100050627     C                   enddo
010200050627     C*
010300050627     C                   ENDSR
010400050627     C***
010500990908
010600000801
010700910830     C*--------------------------------------------------------
010800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010900910830     C*--------------------------------------------------------
011000040526     C     RWFILE        BEGSR
011100990910     C*
011200990914     C                   if        not %open(tivin00r)
011300990908     C                   open      tivin00r
011400990914     C                   endif
011500021113     C                   if        not %open(fivabwwr)
011600021113     C                   open      fivabwwr
011700990914     C                   endif
011800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011900020305     C                   exsr      prevat
012000010201     C*
012100010202     C                   if        chkcall = '0'
012200010202     C*
012300021113     C                   if        not %open(fivatwwr)
012400021113     C                   open      fivatwwr
012500010201     C                   endif
012600990910     C*
012700010201     C                   clear                   §CTROKVB          5 0
012800020305     C                   clear                   §CTROKVT          5 0
012900000801     C                   clear                   §CTRMO            5 0
013000000801     C                   clear                   §CTRNO            5 0
013100990910     C*
013200921023     C                   DO        *HIVAL
013300990913     C*
013400990915     C                   READ      tivin00r                               70
013500050627     C                   if        vindta > *blanks
013600000613     C                   add       1             rrnum
013700000801     C*
013800000801     C                   if        *in70 = *off
013900000801     C                             and
014000000801     C                             (vinflg = *blanks
014100000801     C                              or vinflg = '0'
014200000801     C                              or vinflg = '2')
014300000801     C*
014400000801     C                   clear                   vinmsg
014500000801     C                   eval      vinflg = '1'
014600010305     C*
014700050628     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014800050628     C                   eval      PiStr=%trim(%subst(vindta:36:15))
014900050628     C                   movel(p)  PiStr         curspe
015000050628     C                   add       1             cntspe
015100050628     C*
015200050628     C                   if        cntspe = 1
015300050628     C* A rottura inizializzo campi di totalizzazione
015400050628     C                   clear                   fivab000
015500050628     C*
015600050628     C                   exsr      impvab                                       => carico VAB
015700050628     C                   exsr      exevat                                       => carico VAT
015800050628     C                   eval      depspe = curspe
015900050628     C                   else
016000050628     C                   if        curspe <> depspe
016100040802     C                   exsr      wrivab
016200050628     C* A rottura inizializzo campi di totalizzazione
016300050628     C                   clear                   fivab000
016400050628     C*
016500050628     C                   exsr      impvab                                       => carico VAB
016600050628     C                   exsr      exevat                                       => carico VAT
016700050628     C                   eval      depspe = curspe
016800050628     C                   else
016900050628     C                   exsr      impvab                                       => carico VAB
017000050628     C                   exsr      exevat                                       => carico VAT
017100050628     C                   endif
017200050628     C                   endif
017300000905     C*
017400000905     C                   else
017500000905     C                   eval      vinflg = '1'
017600050628     C                   endif
017700000905     C                   endif
017800000905     C*
017900000905     C  N70              update    tivin000
018000000905     C*
018100991022     C  N70              ENDdo
018200040823     C*
018300040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
018400040823     C                   exsr      wrivab
018500010202     C*
018600010202     C                   endif
018700990910
018800990910     C* Se non ci sono record con errori ...
018900000710     C                   if        §ctrno = 0
019000990910     C* ... restituisco esito OK.
019100990921     C                   eval      wrkesito = '0'
019200990910     C                   else
019300010201     C                   if        §ctrokvb > 0
019400990921     C                   eval      wrkesito = '1'
019500000710     C                   else
019600000710     C                   eval      wrkesito = '2'
019700990910     C                   endif
019800000710     C                   endif
019900990910     C*
020000990914     C                   if        %open(tivin00r)
020100990908     C                   close     tivin00r
020200990914     C                   endif
020300021113     C                   if        %open(fivabwwr)
020400021113     C                   close     fivabwwr
020500990914     C                   endif
020600021113     C                   if        %open(fivatwwr)
020700021113     C                   close     fivatwwr
020800010201     C                   endif
020900990910     C*
021000010201     C                   if        §ctrokvb > 0
021100000724     C                             and vlrpoi <> *zeros
021200010202     C                   exsr      invio
021300990920     C                   endif
021400990920     C*
021500910830     C                   ENDSR
021600000613     C***
021700010305
021800010305     C*----------------------------------------------------*
021900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022000010305     C*----------------------------------------------------*
022100020305     C     WRIVAB        BEGSR
022200050628
022300050628      * imposto il peso totale della spedizione
022400050628     C                   Eval(h)   vabpkb = wpkb
022500050628     C                   Z-ADD     *zeros        wpkb
022600010305     C*
022700021113     C                   write     fivab000                                     => scarico il VAB
022800010305     C*
022900010305     C                   ENDSR
023000990920
023100000801     C*----------------------------------------------------*
023200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023300000801     C*----------------------------------------------------*
023400010201     C     INZVAR        BEGSR
023500000801     C*
023600040802     C                   Z-ADD     *zeros        Num5_0            5 0
023700040802     C                   MOVEL     '0'           FlgCAS            1
023800000801     C*
023900000801     C                   ENDSR
024000000801     C*----------------------------------------------------*
024100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024200000801     C*----------------------------------------------------*
024300000801     C     DEFCAM        BEGSR
024400131127     C*
024500131127     C                   MOVEL     *blanks       wChkCAP           1
024600000801     C*
024700020619     C* Imposto i valori di default...
024800050627     C                   Z-ADD     0110056       VABCCM
024900050627     C                   Z-ADD     0110056       VATCCM
025000050627     C                   Z-ADD     011           VABLNP
025100050627     C                   Z-ADD     011           VATLNP
025200040714     C                   Z-ADD     000           VABCTR
025300131113     C                   MOVEL     '7Q'          VABCTM
025400040823     C                   MOVEL     '1'           VABCBO
025500020619     C* ... e poi verifico se sono stati passati come parametri
025600020619     C                   IF        vlrppt > *blanks
025700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025900020619     C                   EXSR      CHKNUM
026000020619     C                   IF        PiInt=*on
026100020619     C                   Z-ADD     PiVal         VABCCM
026200020619     C                   Z-ADD     PiVal         VATCCM
026300020619     C                   ENDIF
026400040506     C                   ENDIF
026500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026700020619     C                   EXSR      CHKNUM
026800020619     C                   IF        PiInt=*on
026900020619     C                   Z-ADD     PiVal         VABLNP
027000020619     C                   Z-ADD     PiVal         VATLNP
027100040506     C                   ENDIF
027200020619     C                   ENDIF
027300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027500020619     C                   EXSR      CHKNUM
027600020619     C                   IF        PiInt=*on
027700020619     C                   Z-ADD     PiVal         VABCTR
027800040506     C                   ENDIF
027900020619     C                   ENDIF
028000131113     C                   IF        %subst(vlrppt:14:2) <> *blanks
028100131113     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
028200131113     C                   ENDIF
028300131127     C                   IF        %subst(vlrppt:16:1) <> *blanks
028400131127     C                   EVAL      wChkCAP = %trim(%subst(vlrppt:16:1))
028500131127     C                   ENDIF
028600020619     C                   ENDIF
028700000801     C*
028800000801     C                   ENDSR
028900000801     C*----------------------------------------------------*
029000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029100000801     C*----------------------------------------------------*
029200040823     C     IMPVAB        BEGSR
029300040823     C*
029400020305     C                   EXSR      INZVAR
029500020305     C                   EXSR      DEFCAM
029600010305     C*
029700000801     C                   Z-ADD     *zeros        errore            1 0
029800000830     C                   MOVEL     datcor        VABAAS
029900020305     C                   MOVEL     datcor        VATAAS
030000040526     C                   MOVE      datcor        VABMGS
030100040823     C                   MOVE(P)   vlrpoi        VABFGS
030200040823     C                   MOVE(P)   vlrpoi        VATFGS
030300050628     C*
030400050627     C                   EVAL      VABRSD=%trim(%subst(vindta:278:35))
030500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030700020117     C* ==
030800050627     C                   EVAL      VABIND=%trim(%subst(vindta:383:35))
030900050627     C                   EVAL      VABLOD=%trim(%subst(vindta:453:35))
031000050627     C                   EVAL      VABRMA=%trim(%subst(vindta:36:15))
031100050627     C* ......VABNZD (conversone da formato *ISO a formato "BARTOLINI"
031200050627     C                   EVAL      VABNZD=%trim(%subst(vindta:499:3))
031300050627     C                   Z-ADD     1             jNAZ
031400050627     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
031500050627     C                   IF        %found
031600050627     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
031700050627     C                   ENDIF
031800050628     C* CAP
031900131127     C                   IF        wChkCAP <> 'N'
032000050627     C                   EVAL      PiStr=%trim(%subst(vindta:488:11))
032100010201     C                   EXSR      CHKNUM
032200010201     C                   IF        PiInt=*on
032300010201     C                   Z-ADD     PiVal         Num5_0
032400040506     C                   MOVEL(P)  Num5_0        VABCAD
032500010201     C                   ELSE
032600040506     C                   ADD       1             errore
032700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032800040506     C                             + ' ' + 'VABCAD'
032900010201     C                   ENDIF
033000131127     C                   ELSE
033100131127     C                   EVAL      VABCAD=%trim(%subst(vindta:488:11))
033200131127     C                   ENDIF
033300040506     C* Reperisco la provincia dal CAP e dalla località
033400040526     C                   IF        VABCAD <> *blanks AND
033500040526     C                             VABPRD  = *blanks
033600040506     C                   CLEAR                   TISI95DS
033700040506     C                   EVAL      I95TCN = '3'
033800040506     C                   Z-ADD     datcor        I95DAT
033900040506     C                   EVAL      I95CAP = VABCAD
034000040506     C                   EVAL      I95LOC = VABLOD
034100050627     C                   EVAL      I95NAR = VABNZD
034200040506     C                   CALL      'TISI95R'
034300040506     C                   PARM                    TISI95DS
034400040506     C                   EVAL      VABPRD = O95PRV
034500040506     C                   ENDIF
034600040506     C* NCL
034700050627     C                   EVAL      VABNCL = VABNCL + 1
034800040506     C* PKB
034900050627     C                   EVAL      PiStr=%trim(%subst(vindta:181:7))
035000010201     C                   EXSR      CHKNUM
035100010201     C                   IF        PiNum=*on
035200050628     C                   EVAL(h)   Pival = Pival/100                            * gestisco 2 decim.
035300050628     C                   add       PiVal         wpkb
035400010201     C                   ELSE
035500010201     C                   ADD       1             errore
035600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035700010201     C                             + ' ' + 'VABPKB'
035800010201     C                   ENDIF
035900050627     C* VLB
036000050628     C                   EVAL      PiStr=%trim(%subst(vindta:188:7))
036100050627     C                   EXSR      CHKNUM
036200050627     C                   IF        PiNum=*on
036300050627     C                   EVAL      VABVLB=VABVLB+(PiVal/100)                    * gestisco 2 decim.
036400050627     C                   ELSE
036500050627     C                   ADD       1             errore
036600050627     C                   EVAL      vinmsg = %trimr(vinmsg)
036700050627     C                             + ' ' + 'VABVLB'
036800050627     C                   ENDIF
036900040506     C* NSP/RMN
037000050627     C*     => Stacco un numeratore da AZNUM
037100050628      * a cambio di RMA
037200050628     c                   If        curspe <> depspe
037300050928     C                   EVAL      PiStr=%trim(%subst(vindta:36:15))
037400050928     C                   EXSR      CHKNUM
037500050928     C                   IF        PiInt=*on
037600050928     C                   Z-ADD     PiVal         VABNSP
037700050928     C                   Z-ADD     PiVal         VATNSP
037800050928     C                   Z-ADD     PiVal         VABRMN
037900050928     C                   ELSE
038000050928     C                   ADD       1             errore
038100050928     C                   EVAL      vinmsg = %trimr(vinmsg)
038200050928     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038300050928     C                   ENDIF
038400050928     C**!!!              clear                   TRUL33DS
038500050928     C**!!!              eval      I33OPE = *zeros
038600050928     C**!!!              eval      I33CNU = 302
038700050928     C**!!!              eval      I33NUM = 1
038800050928     C**!!!              movel     TRUL33DS      KPJBU
038900050928     C**!!!              call      'TRUL33R'
039000050928     C**!!!              parm                    KPJBA
039100050928     C**!!!              movel     KPJBU         TRUL33DS
039200050928     C**!!!              if        O33ERR = *zeros
039300050928     C**!!!              z-add     O33NRF        VABNSP
039400050928     C**!!!              z-add     O33NRF        VABRMN
039500050928     C**!!!              z-add     O33NRF        VATNSP
039600050928     C**!!!              else
039700050928     C**!!!              Z-ADD     1             errore
039800050928     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
039900050928     C**!!!                        + ' ' + 'VABNSP VABRMN VATNSP'
040000050928     C**!!!              endif
040100050628     C                   endif
040200010205     C*
040300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040400040802     C                   IF        FlgCAS <> '0'
040500010205     C                   EVAL      VABCBO = '4'
040600010205     C                   ELSE
040700010205     C                   EVAL      VABCBO = '1'
040800010205     C                   ENDIF
040900020305     C*
041000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041100011113     C                   EXSR      CHKIMPDIV
041200010202     C*
041300000801     C* Ebbene...
041400000801     C                   ADD       1             §CTRMO
041500010201     C                   IF        errore <> *zeros
041600000801     C                   ADD       1             §CTRNO
041700000801     C                   EVAL      vinflg = '2'
041800000801     C                   ELSE
041900010201     C                   ADD       1             §CTROKVB
042000000801     C                   ENDIF
042100000801     C*
042200000801     C                   ENDSR
042300050628     C*----------------------------------------------------*
042400050628     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
042500050628     C*----------------------------------------------------*
042600050628     C     EXEVAT        BEGSR
042700050628     C*
042800050628     C                   EXSR      INZVAR
042900050628     C                   EXSR      DEFCAM
043000050628     C*
043100050628     C* Valorizzo il barcode
043200050628     C                   EVAL      VATTRC='E'
043300050628     C                   EVAL      VATNOT = %trim(%subst(vindta:1:35))
043400050628     C                   exsr      wrivat                                       => scarico VAT
043500050628     C*
043600050628     C                   ENDSR
043700010201     C*----------------------------------------------------*
043800040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043900010201     C*----------------------------------------------------*
044000020305     C     WRIVAT        BEGSR
044100050628     C*
044200040802     C                   WRITE     FIVAT000
044300010201     C*
044400010201     C                   ENDSR
044500010202     C*----------------------------------------------------*
044600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044700010202     C*----------------------------------------------------*
044800020305     C     PREVAT        BEGSR
044900010202     C*
045000021113     C* Compongo il nome del membro da dare al FIVATWWR
045100010202     C                   eval      parmbr = vlrhdl
045200010202     C                   movel     'M'           parmbr
045300050627     C                   eval      parccm = %subst(vlrKSC:2:7)
045400010202     C                   eval      paropz = '1'
045500010202     C* Effettuo la chiamata al CLLE preposto
045600040506     C                   call(e)   'TITVVTC'
045700010202     C                   parm                    parccm
045800010202     C                   parm                    parmbr
045900010202     C                   parm                    paropz
046000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046100010202     C                   if        %error
046200010202     C                   movel     '1'           chkcall
046300010202     C                   else
046400010202     C                   movel     '0'           chkcall
046500010202     C                   endif
046600010202     C*
046700010202     C                   ENDSR
046800000801     C*----------------------------------------------------*
046900000801     C*  CONTROLLO NUMERICITA' CAMPI
047000000801     C*----------------------------------------------------*
047100000801     C     CHKNUM        BEGSR
047200000801     C*
047300000801     C                   call(e)   'ISNUMERIC'
047400000801     C                   PARM                    PiStr            30
047500050627     C                   PARM      ','           PiDecChr          1
047600000801     C                   PARM      *ZEROS        PiVal            30 9
047700000801     C                   PARM      '0'           PiInt             1
047800000801     C                   PARM      '0'           PiNum             1
047900000801     C                   IF        %error
048000000801     C                   EVAL      PiInt=*off
048100000801     C                   ENDIF
048200000801     C*
048300000801     C                   ENDSR
048400000801     C***
048500000801
048600011113
048700011113     C*----------------------------------------------------*
048800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048900011113     C*----------------------------------------------------*
049000011113     C     CHKIMPDIV     BEGSR
049100011113     C*
049200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049300011113     C                   Z-ADD     *zeros        wrkDec            9 9
049400011113     C*
049500011113     C* Come prima cosa effettuo considerazioni sulla divisa
049600011113     C                   IF        vabIAS > *zeros
049700011113     C                   IF        vabVAS <> 'EUR'
049800011113     C                   EVAL      vabVAS =  'ITL'
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C                   IF        vabCAS > *zeros
050300011113     C                   IF        vabVCA <> 'EUR'
050400011113     C                   EVAL      vabVCA =  'ITL'
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C                   IF        vabVMD > *zeros
050900020305     C                   IF        vabVAD <> 'EUR'
051000011113     C                   EVAL      vabVAD =  'ITL'
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051500011113     C                   Z-ADD     vabIAS        wrkDec
051600011113     C                   IF        wrkDec > *zeros
051700011113     C                   IF        vabVAS = 'ITL'
051800011113     C                   EVAL      vabIAS = *zeros
051900011113     C                   ENDIF
052000011113     C                   ENDIF
052100011113     C*
052200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052300011113     C                   Z-ADD     vabCAS        wrkDec
052400011113     C                   IF        wrkDec > *zeros
052500011113     C                   IF        vabVCA = 'ITL'
052600011113     C                   EVAL      vabCAS = *zeros
052700011113     C                   ENDIF
052800011113     C                   ENDIF
052900011113     C*
053000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053100011113     C                   Z-ADD     vabVMD        wrkDec
053200011113     C                   IF        wrkDec > *zeros
053300011113     C                   IF        vabVAD = 'ITL'
053400011113     C                   EVAL      vabVMD = *zeros
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C                   ENDSR
053900011113     C***
054000011113
054100011113
054200000801
054300000801
054400990920      /TITLE Invio dei dati al punto operativo.
054500010202     C     invio         BEGSR
054600990920     C*
054700021113     C* 1° invio FIVAT
054800010201     C                   reset                   dscmz
054900010201     C                   move      vlrpoi        cmzdst
055000021113     C                   eval      cmzfld = 'FIVATWWR'
055100010201     C                   eval      cmzmbd = vlrhdl
055200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055300021009     C***                if        prmfir = *blanks
055400021113     C                   eval      cmzfla = 'FIVAT00F'
055500021113     C                   eval      cmzmba = 'FIVAT00F'
055600021009     C***                else
055700021009     C***                eval      cmzfla = prmfir
055800021009     C***                eval      cmzmba = prmfir
055900021009     C***                endif
056000010201     C                   eval      cmznrr = *zeros
056100020305     C                   move      §ctrokvt      cmznrr
056200021018     C                   eval      cmzlba = vlrfl1
056300010201     C                   call(e)   'TIS711C'
056400010201     C                   parm                    dscmz
056500010201     C                   parm      *blanks       esito
056600010205     C                   if        %error
056700010205     C                             or cmzerr = '1'
056800010205     C                             or esito  = '1'
056900010205     C                   eval      wrkesito = '3'
057000010205     C                   else
057100010201     C*
057200021113     C* 2° invio FIVAB
057300010201     C                   reset                   dscmz
057400010201     C                   move      vlrpoi        cmzdst
057500010201     C                   eval      cmzfld = vlrfou
057600010201     C                   eval      cmzmbd = vlrhdl
057700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057800021009     C***                if        prmfir = *blanks
057900021113     C                   eval      cmzfla = 'FIVAB00F'
058000021113     C                   eval      cmzmba = 'FIVAB00F'
058100021009     C***                else
058200021009     C***                eval      cmzfla = prmfir
058300021009     C***                eval      cmzmba = prmfir
058400021009     C***                endif
058500010201     C                   eval      cmznrr = *zeros
058600010201     C                   move      §ctrokvb      cmznrr
058700021018     C                   eval      cmzlba = vlrfl1
058800010201     C                   call(e)   'TIS711C'
058900010201     C                   parm                    dscmz
059000010201     C                   parm      *blanks       esito
059100010201     C                   if        %error
059200010201     C                             or cmzerr = '1'
059300010201     C                             or esito  = '1'
059400010201     C                   eval      wrkesito = '3'
059500010201     C                   endif
059600010205     C                   endif
059700990920     C*
059800000613     C                   ENDSR
059900000613     C***
060000070411
060100070411     C     *pssr         BEGSR
060200070411     C*
060300070411     C                   if        %open(tivin00r)
060400070411     C                   close     tivin00r
060500070411     C                   endif
060600070411     C                   if        %open(fivabwwr)
060700070411     C                   close     fivabwwr
060800070411     C                   endif
060900070411     C                   if        %open(fivatwwr)
061000070411     C                   close     fivatwwr
061100070411     C                   endif
061200070411     C*
061300070411     C* Effettuo la chiamata al CLLE preposto
061400070411     C                   call(e)   'TITVVTC'
061500070411     C                   parm                    parccm
061600070411     C                   parm                    parmbr
061700070411     C                   parm      '2'           paropz
061800070411     C*
061900070411     C                   eval      wrkesito = '2'
062000070411     C*
062100070411     C                   seton                                        LR
062200070411     C*
062300070411     C                   ENDSR     '*CANCL'
062400070411     C***
062500070411
062600990910
062700000613     C     *inzsr        BEGSR
062800990910     C*
062900990910     C     *entry        plist
063000990920     C                   parm                    tivlrds
063100990921     C                   parm      wrkesito      esito
063200000724     C                   parm                    prmlit
063300000710     C                   parm                    prmfir
063400000613     C*
063500000830     C* CALCOLA LA DATA CORRENTE
063600000830     C                   time                    wn14             14 0
063700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063800000830     C                   z-add     wn8           g08dat
063900000830     C                   z-add     *zeros        g08inv
064000000830     C                   movel     '0'           g08err
064100000830     C                   call      'XSRDA8'
064200000830     C                   parm                    wlbda8
064300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064400000830     C*
064500050627     C* Chiave su TABEL00F - parziale
064600050627     C     KEYtabP       KLIST
064700050627     C                   KFLD                    tblKUT
064800050627     C                   KFLD                    tblCOD
064900000613     C                   ENDSR
065000000613     C***
