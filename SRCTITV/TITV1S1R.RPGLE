000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
001500060725     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500121221     D wNumColloDA     s              5  0 INZ(*zeros)
002600091006     D wNumColloA      s              3A   INZ(*blanks)
002700091006     D wNumColloCor    s              3A   INZ(*blanks)
002800130329     D wNumColloCorT   s              3A   INZ(*blanks)
002900000830
003000000830     D*------------------
003100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200000830     D*------------------
003300000830     D WLBDA8          DS                  INZ
003400000830     D  G08DAT                 1      8  0
003500000830     D  G08INV                 9     16  0
003600000830     D  G08ERR                17     17
003700000830     D  G08TGI                18     22  0
003800000830     D*
003900990908
004000060307     D*------------------
004100060307     D* DS REPERIMENTO NUMERATORE
004200060307     D*------------------
004300060529     D trul33ds      e ds                  inz
004400060307     D*------------------
004500060307     D* DS ARCHITETTURA
004600060307     D*------------------
004700060307     D kpjba         e ds                  inz
004800060307     D*------------------
004900010201
005000091006
005100091006     D*------------------
005200091006     D* LINKING A DEFINIZIONI ESTERNE
005300091006     D*------------------
005400091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005500091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005600091006
005700010201
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000613     C*
006200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006300000613     C*
006400010202     C* Effettuo la chiamata al CLLE preposto
006500040506     C                   call(e)   'TITVVTC'
006600010202     C                   parm                    parccm
006700010202     C                   parm                    parmbr
006800010202     C                   parm      '2'           paropz
006900050201     C*
007000050201     C* Effettuo lancio TISI95 solo x chiusura
007100050201     C                   CLEAR                   TISI95DS
007200050201     C                   EVAL      I95TLA = 'C'
007300050201     C                   CALL      'TISI95R'
007400050201     C                   PARM                    TISI95DS
007500000616     C*
007600000801     C
007700010201     C                   seton                                        LR
007800990908
007900000801
008000910830     C*--------------------------------------------------------
008100040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008200910830     C*--------------------------------------------------------
008300040526     C     RWFILE        BEGSR
008400990910     C*
008500990914     C                   if        not %open(tivin00r)
008600990908     C                   open      tivin00r
008700990914     C                   endif
008800021113     C                   if        not %open(fivabwwr)
008900021113     C                   open      fivabwwr
009000990914     C                   endif
009100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009200020305     C                   exsr      prevat
009300010201     C*
009400010202     C                   if        chkcall = '0'
009500010202     C*
009600021113     C                   if        not %open(fivatwwr)
009700021113     C                   open      fivatwwr
009800010201     C                   endif
009900990910     C*
010000010201     C                   clear                   §CTROKVB          5 0
010100020305     C                   clear                   §CTROKVT          5 0
010200000801     C                   clear                   §CTRMO            5 0
010300000801     C                   clear                   §CTRNO            5 0
010400990910     C*
010500921023     C                   DO        *HIVAL
010600990913     C*
010700990915     C                   READ      tivin00r                               70
010800050627     C                   if        vindta > *blanks
010900000613     C                   add       1             rrnum
011000000801     C*
011100000801     C                   if        *in70 = *off
011200000801     C                             and
011300000801     C                             (vinflg = *blanks
011400000801     C                              or vinflg = '0'
011500000801     C                              or vinflg = '2')
011600000801     C*
011700000801     C                   clear                   vinmsg
011800000801     C                   eval      vinflg = '1'
011900010305     C*
012000050628     C                   clear                   fivab000
012100060725     C                   clear                   fivat000
012200050628     C                   exsr      impvab                                       => carico VAB
012300060725     C                   exsr      wrivab                                       => scarico VAB
012400000905     C*
012500000905     C                   else
012600000905     C                   eval      vinflg = '1'
012700050628     C                   endif
012800000905     C                   endif
012900000905     C*
013000000905     C  N70              update    tivin000
013100000905     C*
013200991022     C  N70              ENDdo
013300010202     C*
013400010202     C                   endif
013500990910
013600990910     C* Se non ci sono record con errori ...
013700000710     C                   if        §ctrno = 0
013800990910     C* ... restituisco esito OK.
013900990921     C                   eval      wrkesito = '0'
014000990910     C                   else
014100010201     C                   if        §ctrokvb > 0
014200990921     C                   eval      wrkesito = '1'
014300000710     C                   else
014400000710     C                   eval      wrkesito = '2'
014500990910     C                   endif
014600000710     C                   endif
014700990910     C*
014800990914     C                   if        %open(tivin00r)
014900990908     C                   close     tivin00r
015000990914     C                   endif
015100021113     C                   if        %open(fivabwwr)
015200021113     C                   close     fivabwwr
015300990914     C                   endif
015400021113     C                   if        %open(fivatwwr)
015500021113     C                   close     fivatwwr
015600010201     C                   endif
015700990910     C*
015800010201     C                   if        §ctrokvb > 0
015900000724     C                             and vlrpoi <> *zeros
016000010202     C                   exsr      invio
016100990920     C                   endif
016200990920     C*
016300910830     C                   ENDSR
016400000613     C***
016500010305
016600010305     C*----------------------------------------------------*
016700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
016800010305     C*----------------------------------------------------*
016900020305     C     WRIVAB        BEGSR
017000010305     C*
017100060225     C* Quindi scarico il buffer del file d testata
017200021113     C                   write     fivab000                                     => scarico il VAB
017300010305     C*
017400010305     C                   ENDSR
017500990920
017600000801     C*----------------------------------------------------*
017700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017800000801     C*----------------------------------------------------*
017900010201     C     INZVAR        BEGSR
018000000801     C*
018100040802     C                   Z-ADD     *zeros        Num5_0            5 0
018200040802     C                   MOVEL     '0'           FlgCAS            1
018300000801     C*
018400000801     C                   ENDSR
018500000801     C*----------------------------------------------------*
018600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
018700000801     C*----------------------------------------------------*
018800000801     C     DEFCAM        BEGSR
018900000801     C*
019000020619     C* Imposto i valori di default...
019100091006     C                   Z-ADD     0590879       VABCCM
019200091006     C                   Z-ADD     0590879       VATCCM
019300091006     C                   Z-ADD     059           VABLNP
019400091006     C                   Z-ADD     059           VATLNP
019500101102     C                   Z-ADD     310           VABCTR
019600060529     C                   MOVEL     '7Q'          VABCTM
019700060725     C                   MOVEL     '1'           VABCBO
019800020619     C* ... e poi verifico se sono stati passati come parametri
019900020619     C                   IF        vlrppt > *blanks
020000040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020200020619     C                   EXSR      CHKNUM
020300020619     C                   IF        PiInt=*on
020400020619     C                   Z-ADD     PiVal         VABCCM
020500020619     C                   Z-ADD     PiVal         VATCCM
020600020619     C                   ENDIF
020700040506     C                   ENDIF
020800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021000020619     C                   EXSR      CHKNUM
021100020619     C                   IF        PiInt=*on
021200020619     C                   Z-ADD     PiVal         VABLNP
021300020619     C                   Z-ADD     PiVal         VATLNP
021400040506     C                   ENDIF
021500020619     C                   ENDIF
021600040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021800020619     C                   EXSR      CHKNUM
021900020619     C                   IF        PiInt=*on
022000020619     C                   Z-ADD     PiVal         VABCTR
022100040506     C                   ENDIF
022200020619     C                   ENDIF
022300020619     C                   ENDIF
022400000801     C*
022500000801     C                   ENDSR
022600000801     C*----------------------------------------------------*
022700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
022800000801     C*----------------------------------------------------*
022900040823     C     IMPVAB        BEGSR
023000040823     C*
023100020305     C                   EXSR      INZVAR
023200020305     C                   EXSR      DEFCAM
023300010305     C*
023400000801     C                   Z-ADD     *zeros        errore            1 0
023500000830     C                   MOVEL     datcor        VABAAS
023600020305     C                   MOVEL     datcor        VATAAS
023700040526     C                   MOVE      datcor        VABMGS
023800060529     C                   MOVE(P)   vlrpoi        VABFGS
023900060529     C                   MOVE(P)   vlrpoi        VATFGS
024000050628     C*
024100091006     C                   EVAL      VABRSD=%trim(%subst(vindta:84:40))
024200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
024300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
024400020117     C* ==
024500091006     C                   EVAL      VABIND=%trim(%subst(vindta:124:40))
024600091006     C                   EVAL      VABLOD=%trim(%subst(vindta:164:40))
024700091006     C                   EVAL      VABRMA=%trim(%subst(vindta:57:15))
024800091214     C                   EVAL      VABTSP=%trim(%subst(vindta:283:1))
024900091214     C                   EVAL      VABFFD=%trim(%subst(vindta:284:1))
025000091214     C                   EVAL      VABTC1=%trim(%subst(vindta:285:1))
025100101223     C                   EVAL      VABTIC=%trim(%subst(vindta:286:2))
025200101223     C                   EVAL      VABNOT=%trim(%subst(vindta:288:35))
025300101223     C                   EVAL      VABNT2=%trim(%subst(vindta:288+35:35))
025400091006     C* CBO
025500091221     C***                SELECT
025600091221     C***                WHEN      %subst(vindta:282:1) = 'D'
025700091221     C***                EVAL      VABCBO = '4'
025800091221     C***                ENDSL
025900091008     C* CTR
026000101102     C***                SELECT
026100101102     C***                WHEN      %subst(vindta:286:1) = *blanks
026200101102     C***                EVAL      VABCTR = 000
026300101102     C***                WHEN      %subst(vindta:286:1) = 'V'
026400101102     C***                EVAL      VABCTR = 010
026500101102     C***                ENDSL
026600091006     C* CCM
026700101102     C                   IF        %subst(vindta:1:7) = '0590879' OR
026800101102     C                             %subst(vindta:1:7) = '0593583' OR
026900101102     C                             %subst(vindta:1:7) = '0340182' OR
027000101102     C                             %subst(vindta:1:7) = '0593994' OR
027100101102     C                             %subst(vindta:1:7) = '0593995' OR
027200101102     C                             %subst(vindta:1:7) = '0594047' OR
027300101102     C                             %subst(vindta:1:7) = '0594348' OR
027400101102     C                             %subst(vindta:1:7) = '0593947' OR
027500101116     C                             %subst(vindta:1:7) = '0594127' OR
027600101116     C                             %subst(vindta:1:7) = '0594381' OR
027700101116     C                             %subst(vindta:1:7) = '0594382' OR
027800110223     C                             %subst(vindta:1:7) = '0594383' OR
027900110223     C                             %subst(vindta:1:7) = '0594510' OR
028000110304     C                             %subst(vindta:1:7) = '0594511' OR
028100110304     C                             %subst(vindta:1:7) = '0340182' OR
028200110304     C                             %subst(vindta:1:7) = '0821482'
028300091006     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
028400091006     C                   EXSR      CHKNUM
028500091006     C                   IF        PiInt=*on
028600091006     C                   Z-ADD     PiVal         VABCCM
028700091021     C                   Z-ADD     PiVal         VATCCM
028800091006     C                   ELSE
028900091006     C                   ADD       1             errore
029000091006     C                   EVAL      vinmsg = %trimr(vinmsg)
029100091021     C                             + ' ' + 'VABCCM VATCCM'
029200091006     C                   ENDIF
029300091006     C                   ENDIF
029400060529     C* RMN
029500110404     C                   EVAL      PiStr=%trim(%subst(vindta:60:8))
029600060529     C                   EXSR      CHKNUM
029700060529     C                   IF        PiInt=*on
029800060529     C                   Z-ADD     PiVal         VABRMN
029900060529     C                   ELSE
030000060529     C                   ADD       1             errore
030100060529     C                   EVAL      vinmsg = %trimr(vinmsg)
030200060529     C                             + ' ' + 'VABRMN'
030300060529     C                   ENDIF
030400060529     C* NSP => Stacco un numeratore da AZNUM
030500060529     C                   clear                   TRUL33DS
030600060529     C                   eval      I33OPE = *zeros
030700060529     C                   eval      I33CNU = 302
030800060529     C                   eval      I33NUM = 1
030900060529     C                   movel     TRUL33DS      KPJBU
031000060529     C                   call      'TRUL33R'
031100060529     C                   parm                    KPJBA
031200060529     C                   movel     KPJBU         TRUL33DS
031300060529     C                   if        O33ERR = *zeros
031400060529     C                   z-add     O33NRF        VABNSP
031500060529     C                   z-add     O33NRF        VATNSP
031600060529     C                   else
031700060529     C                   Z-ADD     1             errore
031800060529     C                   EVAL      vinmsg = %trimr(vinmsg)
031900060529     C                             + ' ' + 'VABNSP VATNSP'
032000060529     C                   endif
032100060529     C* CAD
032200091006     C                   EVAL      PiStr=%trim(%subst(vindta:204:10))
032300010201     C                   EXSR      CHKNUM
032400010201     C                   IF        PiInt=*on
032500010201     C                   Z-ADD     PiVal         Num5_0
032600040506     C                   MOVEL(P)  Num5_0        VABCAD
032700010201     C                   ELSE
032800040506     C                   ADD       1             errore
032900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033000040506     C                             + ' ' + 'VABCAD'
033100010201     C                   ENDIF
033200091006     C*
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
034600091006     C* NCL
034700091006     C                   EVAL      PiStr=%trim(%subst(vindta:257:6))
034800060725     C                   EXSR      CHKNUM
034900060725     C                   IF        PiInt=*on
035000071214     C                   ADD       PiVal         VABNCL
035100060725     C                   ELSE
035200060725     C                   ADD       1             errore
035300060725     C                   EVAL      vinmsg = %trimr(vinmsg)
035400060725     C                             + ' ' + 'VABNCL'
035500060725     C                   ENDIF
035600040506     C* PKB
035700091006     C                   EVAL      PiStr=%trim(%subst(vindta:263:10))
035800010201     C                   EXSR      CHKNUM
035900010201     C                   IF        PiNum=*on
036000060725     C                   Z-ADD(H)  PiVal         VABPKB
036100010201     C                   ELSE
036200010201     C                   ADD       1             errore
036300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036400010201     C                             + ' ' + 'VABPKB'
036500010201     C                   ENDIF
036600091006     C* VLB
036700091006     C                   EVAL      PiStr=%trim(%subst(vindta:273:6))
036800091006     C                   EXSR      CHKNUM
036900091006     C                   IF        PiNum=*on
037000091006     C                   Z-ADD(H)  PiVal         VABVLB
037100091006     C                   ELSE
037200091006     C                   ADD       1             errore
037300091006     C                   EVAL      vinmsg = %trimr(vinmsg)
037400091006     C                             + ' ' + 'VABVLB'
037500091006     C                   ENDIF
037600060725     C* CAS
037700091006     C                   IF        %subst(vindta:363:12)<>*all'0' AND
037800091006     C                             %subst(vindta:363:12)<>*blanks AND
037900091006     C                             %subst(vindta:363:12)<>'000000000,00'
038000071207     C                   EVAL      FlgCAS='1'
038100091006     C                   EVAL      VABVCA='EUR'
038200091006     C                   EVAL      PiStr=%trim(%subst(vindta:363:12))
038300060307     C                   EXSR      CHKNUM
038400060307     C                   IF        PiNum=*on
038500060725     C                   Z-ADD     PiVal         VABCAS
038600060307     C                   ELSE
038700060307     C                   ADD       1             errore
038800060307     C                   EVAL      vinmsg = %trimr(vinmsg)
038900060307     C                             + ' ' + 'VABCAS'
039000060307     C                   ENDIF
039100060725     C                   ENDIF
039200091006     C* IAS
039300120423     C                   IF        %subst(vindta:452:12)<>*all'0' AND
039400120423     C                             %subst(vindta:452:12)<>*blanks AND
039500120423     C                             %subst(vindta:452:12)<>'000000000,00'
039600091006     C                   EVAL      VABVAS='EUR'
039700120423     C                   EVAL      PiStr=%trim(%subst(vindta:452:12))
039800091006     C                   EXSR      CHKNUM
039900091006     C                   IF        PiNum=*on
040000091006     C                   Z-ADD     PiVal         VABIAS
040100091006     C                   ELSE
040200091006     C                   ADD       1             errore
040300091006     C                   EVAL      vinmsg = %trimr(vinmsg)
040400091006     C                             + ' ' + 'VABIAS'
040500091006     C                   ENDIF
040600091006     C                   ENDIF
040700060529     C*
040800091006     C* Gestione estensioni "VAT"
040900060725     C                   exsr      exeVATE
041000091109     C                   exsr      exeVATB
041100010205     C*
041200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041300040802     C                   IF        FlgCAS <> '0'
041400071207     C                   IF        VABCBO = '1'
041500071207     C                   EVAL      VABCBO = '4'
041600071207     C                   ENDIF
041700071207     C                   IF        VABCBO = '2'
041800071207     C                   EVAL      VABCBO = '6'
041900071207     C                   ENDIF
042000071207     C                   ENDIF
042100020305     C*
042200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300011113     C                   EXSR      CHKIMPDIV
042400010202     C*
042500000801     C* Ebbene...
042600000801     C                   ADD       1             §CTRMO
042700010201     C                   IF        errore <> *zeros
042800000801     C                   ADD       1             §CTRNO
042900000801     C                   EVAL      vinflg = '2'
043000000801     C                   ELSE
043100010201     C                   ADD       1             §CTROKVB
043200000801     C                   ENDIF
043300000801     C*
043400000801     C                   ENDSR
043500050628     C*----------------------------------------------------*
043600060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
043700050628     C*----------------------------------------------------*
043800071207     C     EXEVATE       BEGSR
043900071207     C*
044000071207     C* Inizializzo progressivo collo su totale colli della spedizione
044100091006     C                   Z-ADD     1             wNumColloDA
044200091006     C                   MOVE(P)   VABNCL        wNumColloA
044300130329     C                   MOVE(P)   VABNCL        wNumColloCorT
044400071207     C*
044500071207     C* Costriuisco <N> barcode a seconda del numero d colli presenti sulla bolla
044600071207     C                   EVAL      VATTRC = 'E'
044700091006     C                   DOW       wNumColloDA <= VABNCL
044800121221     C***                MOVEL(P)  wNumColloDA   wNumColloCor
044900121221     C                   MOVE(P)   wNumColloDA   wNumColloCor
045000091006     C***                EVAL      VATNOT = %trim(%subst(vindta:1:9))   +
045100091109     C                   EVAL      VATNOT = %trim(%subst(vindta:37:14)) +
045200130329     C                                      wNumColloCor + wNumColloCorT
045300091116     C***                                   wNumColloA
045400071207     C                   EVAL      wNumColloDA = wNumColloDA + 1
045500071207     C                   write     FIVAT000
045600071207     C                   add       1             §CTROKVT
045700071207     C                   ENDDO
045800050628     C*
045900050628     C                   ENDSR
046000091006     C*----------------------------------------------------*
046100091109     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
046200091006     C*----------------------------------------------------*
046300091109     C     EXEVATB       BEGSR
046400091006     C*
046500091109     C                   EVAL      VATTRC = 'B'
046600110704     C                   EVAL      VATNOT = %subst(vindta:428:12)
046700091006     C                   IF        VATNOT <> *blanks
046800091006     C                   write     FIVAT000
046900091006     C                   add       1             §CTROKVT
047000091006     C                   ENDIF
047100091006     C*
047200091006     C                   ENDSR
047300110704     C*----------------------------------------------------*
047400110704     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
047500110704     C*----------------------------------------------------*
047600110704     C     EXEVATA       BEGSR
047700110704     C*
047800110704     C                   EVAL      VATTRC = 'A'
047900110704     C                   EVAL      VATNOT = %subst(vindta:440:12)
048000110704     C                   IF        VATNOT <> *blanks
048100110704     C                   write     FIVAT000
048200110704     C                   add       1             §CTROKVT
048300110704     C                   ENDIF
048400110704     C*
048500110704     C                   ENDSR
048600010202     C*----------------------------------------------------*
048700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048800010202     C*----------------------------------------------------*
048900020305     C     PREVAT        BEGSR
049000010202     C*
049100021113     C* Compongo il nome del membro da dare al FIVATWWR
049200010202     C                   eval      parmbr = vlrhdl
049300010202     C                   movel     'M'           parmbr
049400050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049500010202     C                   eval      paropz = '1'
049600010202     C* Effettuo la chiamata al CLLE preposto
049700040506     C                   call(e)   'TITVVTC'
049800010202     C                   parm                    parccm
049900010202     C                   parm                    parmbr
050000010202     C                   parm                    paropz
050100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050200010202     C                   if        %error
050300010202     C                   movel     '1'           chkcall
050400010202     C                   else
050500010202     C                   movel     '0'           chkcall
050600010202     C                   endif
050700010202     C*
050800010202     C                   ENDSR
050900000801     C*----------------------------------------------------*
051000000801     C*  CONTROLLO NUMERICITA' CAMPI
051100000801     C*----------------------------------------------------*
051200091006     C     CHKNUM        BEGSR
051300091006     C*
051400091006     C                   IF        PiDecChr = *blanks
051500091006     C                   EVAL      PiDecChr = ','
051600091006     C                   ENDIF
051700091006     C*
051800091006     C                   callp(e)  UBISNUM_Check(PiStr
051900091006     C                                          :PiDecChr
052000091006     C                                          :PiVal
052100091006     C                                          :PiNum
052200091006     C                                          :PiInt)
052300091006     C*
052400091006     C                   IF        %error
052500091006     C                   EVAL      PiInt=*off
052600091006     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900000801     C***
053000000801
053100011113
053200011113     C*----------------------------------------------------*
053300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053400011113     C*----------------------------------------------------*
053500011113     C     CHKIMPDIV     BEGSR
053600011113     C*
053700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053800011113     C                   Z-ADD     *zeros        wrkDec            9 9
053900011113     C*
054000011113     C* Come prima cosa effettuo considerazioni sulla divisa
054100011113     C                   IF        vabIAS > *zeros
054200011113     C                   IF        vabVAS <> 'EUR'
054300011113     C                   EVAL      vabVAS =  'ITL'
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   IF        vabCAS > *zeros
054800011113     C                   IF        vabVCA <> 'EUR'
054900011113     C                   EVAL      vabVCA =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabVMD > *zeros
055400020305     C                   IF        vabVAD <> 'EUR'
055500011113     C                   EVAL      vabVAD =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056000011113     C                   Z-ADD     vabIAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAS = 'ITL'
056300011113     C                   EVAL      vabIAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056800011113     C                   Z-ADD     vabCAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVCA = 'ITL'
057100011113     C                   EVAL      vabCAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057600011113     C                   Z-ADD     vabVMD        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVAD = 'ITL'
057900011113     C                   EVAL      vabVMD = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   ENDSR
058400011113     C***
058500011113
058600011113
058700000801
058800000801
058900990920      /TITLE Invio dei dati al punto operativo.
059000010202     C     invio         BEGSR
059100990920     C*
059200021113     C* 1° invio FIVAT
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500021113     C                   eval      cmzfld = 'FIVATWWR'
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900021113     C                   eval      cmzfla = 'FIVAT00F'
060000021113     C                   eval      cmzmba = 'FIVAT00F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600020305     C                   move      §ctrokvt      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010205     C                   if        %error
061200010205     C                             or cmzerr = '1'
061300010205     C                             or esito  = '1'
061400010205     C                   eval      wrkesito = '3'
061500010205     C                   else
061600010201     C*
061700021113     C* 2° invio FIVAB
061800010201     C                   reset                   dscmz
061900010201     C                   move      vlrpoi        cmzdst
062000010201     C                   eval      cmzfld = vlrfou
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400021113     C                   eval      cmzfla = 'FIVAB00F'
062500021113     C                   eval      cmzmba = 'FIVAB00F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100010201     C                   move      §ctrokvb      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010201     C                   if        %error
063700010201     C                             or cmzerr = '1'
063800010201     C                             or esito  = '1'
063900010201     C                   eval      wrkesito = '3'
064000010201     C                   endif
064100010205     C                   endif
064200990920     C*
064300000613     C                   ENDSR
064400000613     C***
064500070411
064600070411     C     *pssr         BEGSR
064700070411     C*
064800070411     C                   if        %open(tivin00r)
064900070411     C                   close     tivin00r
065000070411     C                   endif
065100070411     C                   if        %open(fivabwwr)
065200070411     C                   close     fivabwwr
065300070411     C                   endif
065400070411     C                   if        %open(fivatwwr)
065500070411     C                   close     fivatwwr
065600070411     C                   endif
065700070411     C*
065800070411     C* Effettuo la chiamata al CLLE preposto
065900070411     C                   call(e)   'TITVVTC'
066000070411     C                   parm                    parccm
066100070411     C                   parm                    parmbr
066200070411     C                   parm      '2'           paropz
066300070411     C*
066400070411     C                   eval      wrkesito = '2'
066500070411     C*
066600070411     C                   seton                                        LR
066700070411     C*
066800070411     C                   ENDSR     '*CANCL'
066900070411     C***
067000070411
067100990910
067200000613     C     *inzsr        BEGSR
067300990910     C*
067400990910     C     *entry        plist
067500990920     C                   parm                    tivlrds
067600990921     C                   parm      wrkesito      esito
067700000724     C                   parm                    prmlit
067800000710     C                   parm                    prmfir
067900000613     C*
068000000830     C* CALCOLA LA DATA CORRENTE
068100000830     C                   time                    wn14             14 0
068200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068300000830     C                   z-add     wn8           g08dat
068400000830     C                   z-add     *zeros        g08inv
068500000830     C                   movel     '0'           g08err
068600000830     C                   call      'XSRDA8'
068700000830     C                   parm                    wlbda8
068800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068900000830     C*
069000000613     C                   ENDSR
069100000613     C***
