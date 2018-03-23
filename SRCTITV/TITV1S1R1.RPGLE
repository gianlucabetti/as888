000100101025      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600101025     FEDIVABwr  O    E             DISK    usropn
000700101025     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600060725     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200010202     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600121221     D wNumColloDA     s              5  0 INZ(*zeros)
002700091006     D wNumColloA      s              3A   INZ(*blanks)
002800091006     D wNumColloCor    s              3A   INZ(*blanks)
002900101025     D wNomeFile       s             30A   INZ(*blanks)
003000101025
003100000830
003200000830     D*------------------
003300000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400000830     D*------------------
003500000830     D WLBDA8          DS                  INZ
003600000830     D  G08DAT                 1      8  0
003700000830     D  G08INV                 9     16  0
003800000830     D  G08ERR                17     17
003900000830     D  G08TGI                18     22  0
004000000830     D*
004100990908
004200060307     D*------------------
004300060307     D* DS REPERIMENTO NUMERATORE
004400060307     D*------------------
004500060529     D trul33ds      e ds                  inz
004600060307     D*------------------
004700060307     D* DS ARCHITETTURA
004800060307     D*------------------
004900060307     D kpjba         e ds                  inz
005000060307     D*------------------
005100010201
005200091006
005300091006     D*------------------
005400091006     D* LINKING A DEFINIZIONI ESTERNE
005500091006     D*------------------
005600091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005700091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005800091006
005900010201
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000613     C*
006400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006500000613     C*
006600010202     C* Effettuo la chiamata al CLLE preposto
006700101025     C                   call(e)   'TITVEVTC'
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
008000000801
008100910830     C*--------------------------------------------------------
008200101025     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008300910830     C*--------------------------------------------------------
008400040526     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900101025     C                   if        not %open(edivabwr)
009000101025     C                   open      edivabwr
009100990914     C                   endif
009200101025     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009300020305     C                   exsr      prevat
009400010201     C*
009500010202     C                   if        chkcall = '0'
009600010202     C*
009700101025     C                   if        not %open(edivatwr)
009800101025     C                   open      edivatwr
009900010201     C                   endif
010000990910     C*
010100010201     C                   clear                   §CTROKVB          5 0
010200020305     C                   clear                   §CTROKVT          5 0
010300000801     C                   clear                   §CTRMO            5 0
010400000801     C                   clear                   §CTRNO            5 0
010500990910     C*
010600921023     C                   DO        *HIVAL
010700990913     C*
010800990915     C                   READ      tivin00r                               70
010900050627     C                   if        vindta > *blanks
011000000613     C                   add       1             rrnum
011100000801     C*
011200000801     C                   if        *in70 = *off
011300000801     C                             and
011400000801     C                             (vinflg = *blanks
011500000801     C                              or vinflg = '0'
011600000801     C                              or vinflg = '2')
011700000801     C*
011800000801     C                   clear                   vinmsg
011900000801     C                   eval      vinflg = '1'
012000010305     C*
012100101025     C                   clear                   edivab00
012200101025     C                   clear                   edivat00
012300050628     C                   exsr      impvab                                       => carico VAB
012400060725     C                   exsr      wrivab                                       => scarico VAB
012500000905     C*
012600000905     C                   else
012700000905     C                   eval      vinflg = '1'
012800050628     C                   endif
012900000905     C                   endif
013000000905     C*
013100000905     C  N70              update    tivin000
013200000905     C*
013300991022     C  N70              ENDdo
013400010202     C*
013500010202     C                   endif
013600990910
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
015200101025     C                   if        %open(edivabwr)
015300101025     C                   close     edivabwr
015400990914     C                   endif
015500101025     C                   if        %open(edivatwr)
015600101025     C                   close     edivatwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   if        §ctrokvb > 0
016000000724     C                             and vlrpoi <> *zeros
016100010202     C                   exsr      invio
016200990920     C                   endif
016300990920     C*
016400910830     C                   ENDSR
016500000613     C***
016600010305
016700010305     C*----------------------------------------------------*
016800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016900010305     C*----------------------------------------------------*
017000020305     C     WRIVAB        BEGSR
017100101025     C*
017200101025     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
017300101025     C                   EVAL      VABCMR = %subst(wNomeFile:1:20)
017400101025     C                   EVAL      VABDCM = DATCOR
017500101025     C                   EVAL      VABDTS = DATCOR
017600101025     C                   EVAL      VABHMS = ORACOR
017700101025     C                   EVAL      VABCNT = 1
017800010305     C*
017900060225     C* Quindi scarico il buffer del file d testata
018000101025     C                   write     edivab00                                     => scarico il VAB
018100010305     C*
018200010305     C                   ENDSR
018300101025
018400101025     C*----------------------------------------------------*
018500101025     C*  SCARICAMENTO BUFFER RECORDS VAT
018600101025     C*----------------------------------------------------*
018700101025     C     WRIVAT        BEGSR
018800101025     C*
018900101025     C                   if        vatnot <> *blanks
019000101025     C*
019100101025     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
019200101025     C                   EVAL      VATCMR = %subst(wNomeFile:1:20)
019300101025     C                   EVAL      VATCNT = 1
019400101025     C*
019500101025     C* Quindi scarico il buffer del file d dettaglio
019600101025     C                   write     edivat00
019700101025     C                   add       1             §CTROKVT
019800101025     C                   endif
019900101025     C*
020000101025     C                   ENDSR
020100990920
020200000801     C*----------------------------------------------------*
020300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020400000801     C*----------------------------------------------------*
020500010201     C     INZVAR        BEGSR
020600000801     C*
020700040802     C                   Z-ADD     *zeros        Num5_0            5 0
020800040802     C                   MOVEL     '0'           FlgCAS            1
020900000801     C*
021000000801     C                   ENDSR
021100000801     C*----------------------------------------------------*
021200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
021300000801     C*----------------------------------------------------*
021400000801     C     DEFCAM        BEGSR
021500000801     C*
021600020619     C* Imposto i valori di default...
021700091006     C                   Z-ADD     0590879       VABCCM
021800091006     C                   Z-ADD     0590879       VATCCM
021900091006     C                   Z-ADD     059           VABLNP
022000091006     C                   Z-ADD     059           VATLNP
022100040714     C                   Z-ADD     000           VABCTR
022200060529     C                   MOVEL     '7Q'          VABCTM
022300060725     C                   MOVEL     '1'           VABCBO
022400020619     C* ... e poi verifico se sono stati passati come parametri
022500020619     C                   IF        vlrppt > *blanks
022600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
022700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
022800020619     C                   EXSR      CHKNUM
022900020619     C                   IF        PiInt=*on
023000020619     C                   Z-ADD     PiVal         VABCCM
023100020619     C                   Z-ADD     PiVal         VATCCM
023200020619     C                   ENDIF
023300040506     C                   ENDIF
023400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
023500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023600020619     C                   EXSR      CHKNUM
023700020619     C                   IF        PiInt=*on
023800020619     C                   Z-ADD     PiVal         VABLNP
023900020619     C                   Z-ADD     PiVal         VATLNP
024000040506     C                   ENDIF
024100020619     C                   ENDIF
024200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
024300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024400020619     C                   EXSR      CHKNUM
024500020619     C                   IF        PiInt=*on
024600020619     C                   Z-ADD     PiVal         VABCTR
024700040506     C                   ENDIF
024800020619     C                   ENDIF
024900020619     C                   ENDIF
025000000801     C*
025100000801     C                   ENDSR
025200000801     C*----------------------------------------------------*
025300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025400000801     C*----------------------------------------------------*
025500040823     C     IMPVAB        BEGSR
025600040823     C*
025700020305     C                   EXSR      INZVAR
025800020305     C                   EXSR      DEFCAM
025900010305     C*
026000000801     C                   Z-ADD     *zeros        errore            1 0
026100000830     C                   MOVEL     datcor        VABAAS
026200020305     C                   MOVEL     datcor        VATAAS
026300040526     C                   MOVE      datcor        VABMGS
026400060529     C                   MOVE(P)   vlrpoi        VABFGS
026500060529     C                   MOVE(P)   vlrpoi        VATFGS
026600050628     C*
026700091006     C                   EVAL      VABRSD=%trim(%subst(vindta:84:40))
026800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027000020117     C* ==
027100091006     C                   EVAL      VABIND=%trim(%subst(vindta:124:40))
027200091006     C                   EVAL      VABLOD=%trim(%subst(vindta:164:40))
027300091008     C                   EVAL      VABNOT=%trim(%subst(vindta:287:35))
027400091008     C                   EVAL      VABNT2=%trim(%subst(vindta:287+35:35))
027500091006     C                   EVAL      VABRMA=%trim(%subst(vindta:57:15))
027600091214     C                   EVAL      VABTSP=%trim(%subst(vindta:283:1))
027700091214     C                   EVAL      VABFFD=%trim(%subst(vindta:284:1))
027800091214     C                   EVAL      VABTC1=%trim(%subst(vindta:285:1))
027900091006     C* CBO
028000091221     C***                SELECT
028100091221     C***                WHEN      %subst(vindta:282:1) = 'D'
028200091221     C***                EVAL      VABCBO = '4'
028300091221     C***                ENDSL
028400091008     C* CTR
028500091008     C                   SELECT
028600091008     C                   WHEN      %subst(vindta:286:1) = *blanks
028700091008     C                   EVAL      VABCTR = 000
028800091008     C                   WHEN      %subst(vindta:286:1) = 'V'
028900091008     C                   EVAL      VABCTR = 010
029000091008     C                   ENDSL
029100091006     C* CCM
029200091006     C                   IF        %subst(vindta:1:7) = '0590879' OR
029300100505     C                             %subst(vindta:1:7) = '0593947' OR
029400100505     C                             %subst(vindta:1:7) = '0594127' OR
029500101022     C                             %subst(vindta:1:7) = '0594348' OR
029600100505     C                             %subst(vindta:1:7) = '0594047'
029700091006     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
029800091006     C                   EXSR      CHKNUM
029900091006     C                   IF        PiInt=*on
030000091006     C                   Z-ADD     PiVal         VABCCM
030100091021     C                   Z-ADD     PiVal         VATCCM
030200091006     C                   ELSE
030300091006     C                   ADD       1             errore
030400091006     C                   EVAL      vinmsg = %trimr(vinmsg)
030500091021     C                             + ' ' + 'VABCCM VATCCM'
030600091006     C                   ENDIF
030700091006     C                   ENDIF
030800060529     C* RMN
030900110404     C                   EVAL      PiStr=%trim(%subst(vindta:60:8))
031000060529     C                   EXSR      CHKNUM
031100060529     C                   IF        PiInt=*on
031200060529     C                   Z-ADD     PiVal         VABRMN
031300060529     C                   ELSE
031400060529     C                   ADD       1             errore
031500060529     C                   EVAL      vinmsg = %trimr(vinmsg)
031600060529     C                             + ' ' + 'VABRMN'
031700060529     C                   ENDIF
031800060529     C* NSP => Stacco un numeratore da AZNUM
031900060529     C                   clear                   TRUL33DS
032000060529     C                   eval      I33OPE = *zeros
032100060529     C                   eval      I33CNU = 302
032200060529     C                   eval      I33NUM = 1
032300060529     C                   movel     TRUL33DS      KPJBU
032400060529     C                   call      'TRUL33R'
032500060529     C                   parm                    KPJBA
032600060529     C                   movel     KPJBU         TRUL33DS
032700060529     C                   if        O33ERR = *zeros
032800060529     C                   z-add     O33NRF        VABNSP
032900060529     C                   z-add     O33NRF        VATNSP
033000060529     C                   else
033100060529     C                   Z-ADD     1             errore
033200060529     C                   EVAL      vinmsg = %trimr(vinmsg)
033300060529     C                             + ' ' + 'VABNSP VATNSP'
033400060529     C                   endif
033500060529     C* CAD
033600091006     C                   EVAL      PiStr=%trim(%subst(vindta:204:10))
033700010201     C                   EXSR      CHKNUM
033800010201     C                   IF        PiInt=*on
033900010201     C                   Z-ADD     PiVal         Num5_0
034000040506     C                   MOVEL(P)  Num5_0        VABCAD
034100010201     C                   ELSE
034200040506     C                   ADD       1             errore
034300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034400040506     C                             + ' ' + 'VABCAD'
034500010201     C                   ENDIF
034600091006     C*
034700040506     C* Reperisco la provincia dal CAP e dalla località
034800040526     C                   IF        VABCAD <> *blanks AND
034900040526     C                             VABPRD  = *blanks
035000040506     C                   CLEAR                   TISI95DS
035100040506     C                   EVAL      I95TCN = '3'
035200040506     C                   Z-ADD     datcor        I95DAT
035300040506     C                   EVAL      I95CAP = VABCAD
035400040506     C                   EVAL      I95LOC = VABLOD
035500050627     C                   EVAL      I95NAR = VABNZD
035600040506     C                   CALL      'TISI95R'
035700040506     C                   PARM                    TISI95DS
035800040506     C                   EVAL      VABPRD = O95PRV
035900040506     C                   ENDIF
036000091006     C* NCL
036100091006     C                   EVAL      PiStr=%trim(%subst(vindta:257:6))
036200060725     C                   EXSR      CHKNUM
036300060725     C                   IF        PiInt=*on
036400071214     C                   ADD       PiVal         VABNCL
036500060725     C                   ELSE
036600060725     C                   ADD       1             errore
036700060725     C                   EVAL      vinmsg = %trimr(vinmsg)
036800060725     C                             + ' ' + 'VABNCL'
036900060725     C                   ENDIF
037000040506     C* PKB
037100091006     C                   EVAL      PiStr=%trim(%subst(vindta:263:10))
037200010201     C                   EXSR      CHKNUM
037300010201     C                   IF        PiNum=*on
037400060725     C                   Z-ADD(H)  PiVal         VABPKB
037500010201     C                   ELSE
037600010201     C                   ADD       1             errore
037700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037800010201     C                             + ' ' + 'VABPKB'
037900010201     C                   ENDIF
038000091006     C* VLB
038100091006     C                   EVAL      PiStr=%trim(%subst(vindta:273:6))
038200091006     C                   EXSR      CHKNUM
038300091006     C                   IF        PiNum=*on
038400091006     C                   Z-ADD(H)  PiVal         VABVLB
038500091006     C                   ELSE
038600091006     C                   ADD       1             errore
038700091006     C                   EVAL      vinmsg = %trimr(vinmsg)
038800091006     C                             + ' ' + 'VABVLB'
038900091006     C                   ENDIF
039000060725     C* CAS
039100091006     C                   IF        %subst(vindta:363:12)<>*all'0' AND
039200091006     C                             %subst(vindta:363:12)<>*blanks AND
039300091006     C                             %subst(vindta:363:12)<>'000000000,00'
039400071207     C                   EVAL      FlgCAS='1'
039500091006     C                   EVAL      VABVCA='EUR'
039600091006     C                   EVAL      PiStr=%trim(%subst(vindta:363:12))
039700060307     C                   EXSR      CHKNUM
039800060307     C                   IF        PiNum=*on
039900060725     C                   Z-ADD     PiVal         VABCAS
040000060307     C                   ELSE
040100060307     C                   ADD       1             errore
040200060307     C                   EVAL      vinmsg = %trimr(vinmsg)
040300060307     C                             + ' ' + 'VABCAS'
040400060307     C                   ENDIF
040500060725     C                   ENDIF
040600091006     C* IAS
040700120423     C                   IF        %subst(vindta:452:12)<>*all'0' AND
040800120423     C                             %subst(vindta:452:12)<>*blanks AND
040900120423     C                             %subst(vindta:452:12)<>'000000000,00'
041000091006     C                   EVAL      VABVAS='EUR'
041100120423     C                   EVAL      PiStr=%trim(%subst(vindta:452:12))
041200091006     C                   EXSR      CHKNUM
041300091006     C                   IF        PiNum=*on
041400091006     C                   Z-ADD     PiVal         VABIAS
041500091006     C                   ELSE
041600091006     C                   ADD       1             errore
041700091006     C                   EVAL      vinmsg = %trimr(vinmsg)
041800091006     C                             + ' ' + 'VABIAS'
041900091006     C                   ENDIF
042000091006     C                   ENDIF
042100060529     C*
042200091006     C* Gestione estensioni "VAT"
042300060725     C                   exsr      exeVATE
042400091109     C                   exsr      exeVATB
042500010205     C*
042600010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042700040802     C                   IF        FlgCAS <> '0'
042800071207     C                   IF        VABCBO = '1'
042900071207     C                   EVAL      VABCBO = '4'
043000071207     C                   ENDIF
043100071207     C                   IF        VABCBO = '2'
043200071207     C                   EVAL      VABCBO = '6'
043300071207     C                   ENDIF
043400071207     C                   ENDIF
043500020305     C*
043600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043700011113     C                   EXSR      CHKIMPDIV
043800010202     C*
043900000801     C* Ebbene...
044000000801     C                   ADD       1             §CTRMO
044100010201     C                   IF        errore <> *zeros
044200000801     C                   ADD       1             §CTRNO
044300000801     C                   EVAL      vinflg = '2'
044400000801     C                   ELSE
044500010201     C                   ADD       1             §CTROKVB
044600000801     C                   ENDIF
044700000801     C*
044800000801     C                   ENDSR
044900050628     C*----------------------------------------------------*
045000060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
045100050628     C*----------------------------------------------------*
045200071207     C     EXEVATE       BEGSR
045300071207     C*
045400071207     C* Inizializzo progressivo collo su totale colli della spedizione
045500091006     C                   Z-ADD     1             wNumColloDA
045600091006     C                   MOVE(P)   VABNCL        wNumColloA
045700071207     C*
045800071207     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
045900071207     C                   EVAL      VATTRC = 'E'
046000091006     C                   DOW       wNumColloDA <= VABNCL
046100121221     C***                MOVEL(P)  wNumColloDA   wNumColloCor
046200121221     C                   MOVE(P)   wNumColloDA   wNumColloCor
046300091006     C***                EVAL      VATNOT = %trim(%subst(vindta:1:9))   +
046400091109     C                   EVAL      VATNOT = %trim(%subst(vindta:37:14)) +
046500091116     C                                      wNumColloCor
046600091116     C***                                   wNumColloA
046700071207     C                   EVAL      wNumColloDA = wNumColloDA + 1
046800101025     C                   exsr      wrivat
046900071207     C                   ENDDO
047000050628     C*
047100050628     C                   ENDSR
047200091006     C*----------------------------------------------------*
047300091109     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
047400091006     C*----------------------------------------------------*
047500091109     C     EXEVATB       BEGSR
047600091006     C*
047700091109     C                   EVAL      VATTRC = 'B'
047800091006     C                   EVAL      VATNOT = %subst(vindta:428:10)
047900091006     C                   IF        VATNOT <> *blanks
048000101025     C                   exsr      wrivat
048100091006     C                   ENDIF
048200091006     C*
048300091006     C                   ENDSR
048400010202     C*----------------------------------------------------*
048500101025     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048600010202     C*----------------------------------------------------*
048700020305     C     PREVAT        BEGSR
048800010202     C*
048900101025     C* Compongo il nome del membro da dare al EDIVATWR
049000010202     C                   eval      parmbr = vlrhdl
049100010202     C                   movel     'M'           parmbr
049200050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049300010202     C                   eval      paropz = '1'
049400010202     C* Effettuo la chiamata al CLLE preposto
049500101025     C                   call(e)   'TITVEVTC'
049600010202     C                   parm                    parccm
049700010202     C                   parm                    parmbr
049800010202     C                   parm                    paropz
049900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050000010202     C                   if        %error
050100010202     C                   movel     '1'           chkcall
050200010202     C                   else
050300010202     C                   movel     '0'           chkcall
050400010202     C                   endif
050500010202     C*
050600010202     C                   ENDSR
050700000801     C*----------------------------------------------------*
050800000801     C*  CONTROLLO NUMERICITA' CAMPI
050900000801     C*----------------------------------------------------*
051000091006     C     CHKNUM        BEGSR
051100091006     C*
051200091006     C                   IF        PiDecChr = *blanks
051300091006     C                   EVAL      PiDecChr = ','
051400091006     C                   ENDIF
051500091006     C*
051600091006     C                   callp(e)  UBISNUM_Check(PiStr
051700091006     C                                          :PiDecChr
051800091006     C                                          :PiVal
051900091006     C                                          :PiNum
052000091006     C                                          :PiInt)
052100091006     C*
052200091006     C                   IF        %error
052300091006     C                   EVAL      PiInt=*off
052400091006     C                   ENDIF
052500000801     C*
052600000801     C                   ENDSR
052700000801     C***
052800000801
052900011113
053000011113     C*----------------------------------------------------*
053100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053200011113     C*----------------------------------------------------*
053300011113     C     CHKIMPDIV     BEGSR
053400011113     C*
053500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053600011113     C                   Z-ADD     *zeros        wrkDec            9 9
053700011113     C*
053800011113     C* Come prima cosa effettuo considerazioni sulla divisa
053900011113     C                   IF        vabIAS > *zeros
054000011113     C                   IF        vabVAS <> 'EUR'
054100011113     C                   EVAL      vabVAS =  'ITL'
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C                   IF        vabCAS > *zeros
054600011113     C                   IF        vabVCA <> 'EUR'
054700011113     C                   EVAL      vabVCA =  'ITL'
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C                   IF        vabVMD > *zeros
055200020305     C                   IF        vabVAD <> 'EUR'
055300011113     C                   EVAL      vabVAD =  'ITL'
055400011113     C                   ENDIF
055500011113     C                   ENDIF
055600011113     C*
055700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055800011113     C                   Z-ADD     vabIAS        wrkDec
055900011113     C                   IF        wrkDec > *zeros
056000011113     C                   IF        vabVAS = 'ITL'
056100011113     C                   EVAL      vabIAS = *zeros
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056600011113     C                   Z-ADD     vabCAS        wrkDec
056700011113     C                   IF        wrkDec > *zeros
056800011113     C                   IF        vabVCA = 'ITL'
056900011113     C                   EVAL      vabCAS = *zeros
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057400011113     C                   Z-ADD     vabVMD        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVAD = 'ITL'
057700011113     C                   EVAL      vabVMD = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C                   ENDSR
058200011113     C***
058300011113
058400011113
058500000801
058600000801
058700990920      /TITLE Invio dei dati al punto operativo.
058800010202     C     invio         BEGSR
058900990920     C*
059000021113     C* 1° invio FIVAT
059100010201     C                   reset                   dscmz
059200010201     C                   move      vlrpoi        cmzdst
059300101025     C                   eval      cmzfld = 'EDIVATWR'
059400010201     C                   eval      cmzmbd = vlrhdl
059500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059600021009     C***                if        prmfir = *blanks
059700101025     C                   eval      cmzfla = 'EDIVAT0F'
059800101025     C                   eval      cmzmba = 'EDIVAT0F'
059900021009     C***                else
060000021009     C***                eval      cmzfla = prmfir
060100021009     C***                eval      cmzmba = prmfir
060200021009     C***                endif
060300010201     C                   eval      cmznrr = *zeros
060400020305     C                   move      §ctrokvt      cmznrr
060500021018     C                   eval      cmzlba = vlrfl1
060600010201     C                   call(e)   'TIS711C'
060700010201     C                   parm                    dscmz
060800010201     C                   parm      *blanks       esito
060900010205     C                   if        %error
061000010205     C                             or cmzerr = '1'
061100010205     C                             or esito  = '1'
061200010205     C                   eval      wrkesito = '3'
061300010205     C                   else
061400010201     C*
061500021113     C* 2° invio FIVAB
061600010201     C                   reset                   dscmz
061700010201     C                   move      vlrpoi        cmzdst
061800010201     C                   eval      cmzfld = vlrfou
061900010201     C                   eval      cmzmbd = vlrhdl
062000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062100021009     C***                if        prmfir = *blanks
062200101025     C                   eval      cmzfla = 'EDIVAB0F'
062300101025     C                   eval      cmzmba = 'EDIVAB0F'
062400021009     C***                else
062500021009     C***                eval      cmzfla = prmfir
062600021009     C***                eval      cmzmba = prmfir
062700021009     C***                endif
062800010201     C                   eval      cmznrr = *zeros
062900010201     C                   move      §ctrokvb      cmznrr
063000021018     C                   eval      cmzlba = vlrfl1
063100010201     C                   call(e)   'TIS711C'
063200010201     C                   parm                    dscmz
063300010201     C                   parm      *blanks       esito
063400010201     C                   if        %error
063500010201     C                             or cmzerr = '1'
063600010201     C                             or esito  = '1'
063700010201     C                   eval      wrkesito = '3'
063800010201     C                   endif
063900010205     C                   endif
064000990920     C*
064100000613     C                   ENDSR
064200000613     C***
064300070411
064400070411     C     *pssr         BEGSR
064500070411     C*
064600070411     C                   if        %open(tivin00r)
064700070411     C                   close     tivin00r
064800070411     C                   endif
064900101025     C                   if        %open(edivabwr)
065000101025     C                   close     edivabwr
065100070411     C                   endif
065200101025     C                   if        %open(edivatwr)
065300101025     C                   close     edivatwr
065400070411     C                   endif
065500070411     C*
065600070411     C* Effettuo la chiamata al CLLE preposto
065700101025     C                   call(e)   'TITVEVTC'
065800070411     C                   parm                    parccm
065900070411     C                   parm                    parmbr
066000070411     C                   parm      '2'           paropz
066100070411     C*
066200070411     C                   eval      wrkesito = '2'
066300070411     C*
066400070411     C                   seton                                        LR
066500070411     C*
066600070411     C                   ENDSR     '*CANCL'
066700070411     C***
066800070411
066900990910
067000000613     C     *inzsr        BEGSR
067100990910     C*
067200990910     C     *entry        plist
067300990920     C                   parm                    tivlrds
067400990921     C                   parm      wrkesito      esito
067500000724     C                   parm                    prmlit
067600000710     C                   parm                    prmfir
067700101025     C*
067800101025     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
067900101025     C                   move(p)   vlrMSG        wNomeFile
068000000613     C*
068100000830     C* CALCOLA LA DATA CORRENTE
068200000830     C                   time                    wn14             14 0
068300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068400101025     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
068500000830     C                   z-add     wn8           g08dat
068600000830     C                   z-add     *zeros        g08inv
068700000830     C                   movel     '0'           g08err
068800000830     C                   call      'XSRDA8'
068900000830     C                   parm                    wlbda8
069000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069100000830     C*
069200000613     C                   ENDSR
069300000613     C***
