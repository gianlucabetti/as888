000100070808      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130805     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600070808     FEDIVABwr  O    E             DISK    usropn
000700070808     FEDIVATwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070719     D tisi95ds      e ds
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100990915     D wrkdata         s               d
002200990915     D wrkora          s               t
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400000621     D recko           s            150    INZ(*blanks)
002500011113     D depcmd          s            150    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000070906     D wCMR            s             35    INZ(*blanks)
003100130805     D wNOTE           s             70    INZ(*blanks)
003200070919     D wPrimaBolla     s              1    INZ('0')
003300000830
003400041025     D*------------------
003500041025     D* DS REPERIMENTO NUMERATORE
003600041025     D*------------------
003700041025     D trul33ds      e ds                  inz
003800041025     D*------------------
003900041025     D* DS ARCHITETTURA
004000041025     D*------------------
004100041025     D kpjba         e ds                  inz
004200041025     D*------------------
004300990908
004400131105     D*
004500131105     D TxtInOut        S           2048
004600131105     D ElencoChar      S            256    inz('QWERTYUIOPASDFGHJKLZXCVBNMqwert-
004700131105     D                                     yuiopasdfghjklzxcvbnm.')
004800131105     D TipoElenco      S              1
004900131105     D CharSost        S              1
005000131105     D UpperCase       S              1
005100131105     D ChkNull         S              1
005200131105     D CharNull        S              1
005300131105     D EsitoBon        S              1
005400010201
005500010201
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000613     C*
006200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006300070719     C*
006400070719     C* Esegue lancio TISI95R solo x chiusura
006500070719     C                   CLEAR                   TISI95DS
006600070719     C                   EVAL      I95TLA = 'C'
006700070719     C                   CALL      'TISI95R'
006800070719     C                   PARM                    TISI95DS
006900000613     C*
007000010202     C* Effettuo la chiamata al CLLE preposto
007100070808     C                   call(e)   'TITVEVTC'
007200010202     C                   parm                    parccm
007300010202     C                   parm                    parmbr
007400010202     C                   parm      '2'           paropz
007500000616     C*
007600010201     C                   seton                                        LR
007700990908
007800000801
007900910830     C*--------------------------------------------------------
008000070808     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008100910830     C*--------------------------------------------------------
008200040526     C     RWFILE        BEGSR
008300990910     C*
008400990914     C                   if        not %open(tivin00r)
008500990908     C                   open      tivin00r
008600990914     C                   endif
008700070808     C                   if        not %open(edivabwr)
008800070808     C                   open      edivabwr
008900990914     C                   endif
009000070808     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009100020305     C                   exsr      prevat
009200010201     C*
009300010202     C                   if        chkcall = '0'
009400010202     C*
009500070808     C                   if        not %open(edivatwr)
009600070808     C                   open      edivatwr
009700010201     C                   endif
009800990910     C*
009900010201     C                   clear                   §CTROKVB          5 0
010000020305     C                   clear                   §CTROKVT          5 0
010100000801     C                   clear                   §CTRMO            5 0
010200000801     C                   clear                   §CTRNO            5 0
010300070919     C*
010400070919     C* Inizializzazioni iniziali e formati record file Bartolini
010500070919     C                   EXSR      INZVAR
010600070919     C                   EXSR      DEFCAM
010700040910     C*
010800921023     C                   DO        *HIVAL
010900990913     C*
011000990915     C                   READ      tivin00r                               70
011100040910     C                   if        vindta > *blanks
011200000613     C                   add       1             rrnum
011300000801     C*
011400000801     C                   if        *in70 = *off
011500000801     C                             and
011600000801     C                             (vinflg = *blanks
011700000801     C                              or vinflg = '0'
011800000801     C                              or vinflg = '2')
011900000801     C*
012000000801     C                   clear                   vinmsg
012100000801     C                   eval      vinflg = '1'
012200040910     C*
012300040910     C* Eseguo routine d traduzione
012400040910     C                   exsr      impvabvat
012500040802     C*
012600010305     C                   endif
012700000905     C*
012800000905     C                   else
012900000905     C                   eval      vinflg = '1'
013000000905     C                   endif
013100000905     C*
013200000905     C  N70              update    tivin000
013300000905     C*
013400991022     C  N70              ENDdo
013500010202     C*
013600070919     C* Scarico l'ultima bolla rimasta in canna
013700070919     C                   IF        wPrimaBolla = '1'
013800130805     C                   EXSR      VALVABCMR
013900131105     C                   EXSR      WRIVAB
014000070919     C                   ENDIF
014100070919     C*
014200010202     C                   endif
014300990910
014400990910     C* Se non ci sono record con errori ...
014500000710     C                   if        §ctrno = 0
014600990910     C* ... restituisco esito OK.
014700990921     C                   eval      wrkesito = '0'
014800990910     C                   else
014900010201     C                   if        §ctrokvb > 0
015000990921     C                   eval      wrkesito = '1'
015100000710     C                   else
015200000710     C                   eval      wrkesito = '2'
015300990910     C                   endif
015400000710     C                   endif
015500990910     C*
015600990914     C                   if        %open(tivin00r)
015700990908     C                   close     tivin00r
015800990914     C                   endif
015900070808     C                   if        %open(edivabwr)
016000070808     C                   close     edivabwr
016100990914     C                   endif
016200070808     C                   if        %open(edivatwr)
016300070808     C                   close     edivatwr
016400010201     C                   endif
016500990910     C*
016600010201     C                   if        §ctrokvb > 0
016700000724     C                             and vlrpoi <> *zeros
016800010202     C                   exsr      invio
016900990920     C                   endif
017000990920     C*
017100910830     C                   ENDSR
017200000613     C***
017300990920
017400000801     C*----------------------------------------------------*
017500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017600000801     C*----------------------------------------------------*
017700010201     C     INZVAR        BEGSR
017800000801     C*
017900130805     C                   MOVEL     *blanks       wNOTE
018000040802     C                   Z-ADD     *zeros        Num5_0            5 0
018100040802     C                   MOVEL     '0'           FlgCAS            1
018200000801     C*
018300000801     C                   ENDSR
018400000801     C*----------------------------------------------------*
018500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
018600000801     C*----------------------------------------------------*
018700000801     C     DEFCAM        BEGSR
018800000801     C*
018900070808     C                   CLEAR                   EDIVAB00
019000070808     C                   CLEAR                   EDIVAT00
019100020619     C* Imposto i valori di default...
019200131120     C                   Z-ADD     0436269       VABCCM
019300131120     C                   Z-ADD     0436269       VATCCM
019400130808     C                   Z-ADD     043           VABLNP
019500130808     C                   Z-ADD     043           VATLNP
019600130808     C                   Z-ADD     304           VABCTR
019700080825     C                   MOVEL     '7Q'          VABCTM
019800040823     C                   MOVEL     '1'           VABCBO
019900020619     C* ... e poi verifico se sono stati passati come parametri
020000020619     C                   IF        vlrppt > *blanks
020100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
020200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
020300020619     C                   EXSR      CHKNUM
020400020619     C                   IF        PiInt=*on
020500020619     C                   Z-ADD     PiVal         VABCCM
020600020619     C                   Z-ADD     PiVal         VATCCM
020700020619     C                   ENDIF
020800040506     C                   ENDIF
020900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
021000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
021100020619     C                   EXSR      CHKNUM
021200020619     C                   IF        PiInt=*on
021300020619     C                   Z-ADD     PiVal         VABLNP
021400020619     C                   Z-ADD     PiVal         VATLNP
021500040506     C                   ENDIF
021600020619     C                   ENDIF
021700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021900020619     C                   EXSR      CHKNUM
022000020619     C                   IF        PiInt=*on
022100020619     C                   Z-ADD     PiVal         VABCTR
022200040506     C                   ENDIF
022300020619     C                   ENDIF
022400060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
022500060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
022600060202     C                   ENDIF
022700020619     C                   ENDIF
022800000801     C*
022900000801     C                   ENDSR
023000000801     C*----------------------------------------------------*
023100070808     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
023200000801     C*----------------------------------------------------*
023300130805     C     IMPVABVAT     BEGSR
023400131104     C*
023500131104     C* Forzatura iniziale: se presente coppia carattere ?' => '
023600131104     C     '?''':''' '   xlate     vindta        vindta
023700040910     C*
023800040910     C* Traduzione relativa ai tipi record del file del cliente
023900040910     C*
024000070719     C* Resetto indicatore d anomalia sul singolo record
024100070719     C                   eval      vinflg = '1'
024200130805     C***
024300131120     C* ...tipo record 'A00' (bordero)
024400131120     C                   IF        %subst(vindta:1:3) = 'A00'
024500131120     C                   movel     *blanks       wDATA_BRD         9
024600131120     C                   evalr     wDATA_BRD = %subst(vindta:45:8)
024700131120     C                   eval      wCMR = %trim(%subst(vindta:10:35))
024800131120     C                   move      wDATA_BRD     wCMR
024900130805     C                   ENDIF
025000130805     C***
025100131120     C* ...tipo record 'B00' + 'CON' (testata)
025200131120     C                   IF        %subst(vindta:1:3) = 'B00' AND
025300131120     C                             %subst(vindta:7:3) = 'CON'
025400130805     C* ......verifico se è la prima bolla elaborata
025500130805     C                   IF        wPrimaBolla = '0'
025600130805     C                   EVAL      wPrimaBolla = '1'
025700130805     C                   ELSE
025800130805     C* Considerazioni sul tipo porto
025900130805     C                   IF        FlgCAS <> '0'
026000130805     C                   IF        VABCBO = '1'
026100130805     C                   EVAL      VABCBO = '4'
026200130805     C                   ELSE
026300130805     C                   EVAL      VABCBO = '6'
026400130805     C                   ENDIF
026500130805     C                   ENDIF
026600130805     C* Eseguo routine finale x considerazioni specifiche su importi/divise
026700130805     C                   EXSR      CHKIMPDIV
026800130805     C* Considerazioni finali
026900130805     C                   EXSR      VALVABCMR
027000130805     C* Infine scarico il buffer di output
027100131105     C                   EXSR      WRIVAB
027200130805     C* ......inizializzazioni iniziali e formati record file Bartolini
027300130805     C                   EXSR      INZVAR
027400130805     C                   EXSR      DEFCAM
027500130805     C                   ENDIF
027600130805     C*
027700130805     C                   Z-ADD     *zeros        errore            1 0
027800130805     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
027900130805     C                   MOVEL     datcor        VABAAS
028000130805     C                   MOVEL     datcor        VATAAS
028100130805     C                   MOVE      datcor        VABMGS
028200130805     C                   MOVE(P)   vlrpoi        VABFGS
028300130805     C                   MOVE(P)   vlrpoi        VATFGS
028400130805     C* ......VABNSP/VATNSP
028500130805     C* NSP => Stacco un numeratore da AZNUM
028600130805     C                   clear                   TRUL33DS
028700130805     C                   eval      I33OPE = *zeros
028800130805     C                   eval      I33CNU = 302
028900130805     C                   eval      I33NUM = 1
029000130805     C                   movel     TRUL33DS      KPJBU
029100130805     C                   call      'TRUL33R'
029200130805     C                   parm                    KPJBA
029300130805     C                   movel     KPJBU         TRUL33DS
029400130805     C                   if        O33ERR = *zeros
029500130805     C                   z-add     O33NRF        VABNSP
029600130805     C                   z-add     O33NRF        VATNSP
029700130805     C                   else
029800130805     C                   Z-ADD     1             errore
029900130805     C                   EVAL      vinmsg = %trimr(vinmsg)
030000130805     C                             + ' ' + 'VABNSP VATNSP'
030100130805     C                   endif
030200130805     C* ......VABNSP/VATNSP
030300130805     C*                  EVAL      PiStr=%trim(%subst(vindta:3:10))
030400130805     C*                  EXSR      CHKNUM
030500130805     C*                  IF        PiInt=*on
030600130805     C*                  Z-ADD     PiVal         VABNSP
030700130805     C*                  Z-ADD     PiVal         VATNSP
030800130805     C*                  ELSE
030900130805     C*                  ADD       1             errore
031000130805     C*                  EVAL      vinmsg = %trimr(vinmsg)
031100130805     C*                            + ' ' + 'VABNSP VATNSP'
031200130805     C*                  ENDIF
031300130805     C***
031400130805     C* ......VABRSD
031500131120     C                   EVAL      VABRSD=%trim(%subst(vindta:10:35))
031600131120     C* ......VABNZD
031700131120     C                   EVAL      VABNZD=%trim(%subst(vindta:80:3))
031800131120     C                   IF        VABNZD = 'IT' OR VABNZD = 'I'
031900131120     C                   EVAL      VABNZD = *blanks
032000131120     C                   ENDIF
032100130805     C* ......VABCAD
032200131120     C                   EVAL      VABCAD=%trim(%subst(vindta:83:9))
032300130805     C* ......VABIND
032400131120     C                   EVAL      VABIND=%trim(%subst(vindta:45:35))
032500131120     C* ......VABLOD
032600131120     C                   EVAL      VABLOD=%trim(%subst(vindta:92:35))
032700131120     C* ......VABRD2
032800131120     C                   EVAL      VABRD2=%trim(%subst(vindta:162:35))
032900130805     C* ......VABPRD
033000130805     C* Reperisco la provincia dal CAP e dalla località
033100130805     C                   IF        VABNZD  = *blanks AND
033200130805     C                             VABPRD  = *blanks AND
033300130805     C                             VABCAD <> *blanks AND
033400130805     C                             VABLOD <> *blanks
033500130805     C                   CLEAR                   TISI95DS
033600130805     C                   EVAL      I95TCN = '3'
033700130805     C                   Z-ADD     datcor        I95DAT
033800130805     C                   EVAL      I95CAP = VABCAD
033900130805     C                   EVAL      I95LOC = VABLOD
034000130805     C                   CALL      'TISI95R'
034100130805     C                   PARM                    TISI95DS
034200130805     C                   EVAL      VABPRD = O95PRV
034300130805     C                   ENDIF
034400130805     C*
034500130805     C                   ENDIF
034600131120     C***
034700131120     C* ...tipo record 'D00' (testata)
034800131120     C                   IF        %subst(vindta:1:3) = 'D00'
034900131120     C* ......VABNCL
035000131120     C                   EVAL      PiStr=%trim(%subst(vindta:10:4))
035100131120     C                   EXSR      CHKNUM
035200131120     C                   IF        PiInt=*on
035300131120     C                   ADD       PiVal         VABNCL
035400131120     C                   ELSE
035500131120     C                   ADD       1             errore
035600131120     C                   EVAL      vinmsg = %trimr(vinmsg)
035700131120     C                             + ' ' + 'VABNCL'
035800131120     C                   ENDIF
035900131120     C* ......VABRMA
036000131120     C                   EVAL      VABRMA=%trim(%subst(vindta:59:35))
036100131120     C* ......VABPKB
036200131120     C                   EVAL      PiStr=%trim(%subst(vindta:94:9))
036300131120     C                   EXSR      CHKNUM
036400131120     C                   IF        PiNum=*on
036500131120     C                   EVAL      VABPKB = PiVal / 1000                        * da grammi a kg
036600131120     C                   ELSE
036700131120     C                   ADD       1             errore
036800131120     C                   EVAL      vinmsg = %trimr(vinmsg)
036900131120     C                             + ' ' + 'VABPKB'
037000131120     C                   ENDIF
037100131120     C*
037200131120     C                   ENDIF
037300131120     C***
037400131120     C* ...tipo record 'F00' (dettaglio segnacolli)
037500131120     C                   IF        %subst(vindta:1:3) = 'F00'
037600131120     C* ......VATNOT_E
037700131120     C                   EVAL      VATNOT=%trim(%subst(vindta:10:35))           * 20 long
037800131120     C                   EVAL      VATTRC = 'E'
037900131120     C                   IF        VATNOT <> *blanks
038000131120     C                   EXSR      VALVATCMR
038100131120     C                   WRITE     EDIVAT00
038200131120     C                   ENDIF
038300131120     C*
038400131120     C                   ENDIF
038500130805     C***
038600131120     C* ...tipo record 'G00' (testata)
038700131120     C                   IF        %subst(vindta:1:3) = 'G00'
038800130805     C* ......VABRMN
038900131120     C                   EVAL      PiStr=%trim(%subst(vindta:7:35))
039000130805     C                   EXSR      CHKNUM
039100130805     C                   IF        PiInt=*on
039200130805     C                   Z-ADD     PiVal         VABRMN
039300130805     C                   ELSE
039400130805     C                   ADD       1             errore
039500130805     C                   EVAL      vinmsg = %trimr(vinmsg)
039600130805     C                             + ' ' + 'VABRMN'
039700130805     C                   ENDIF
039800130805     C*
039900130805     C                   ENDIF
040000130806     C***
040100131120     C* ...tipo record 'H00' + '101' (testata - attributi accessori)
040200131121     C                   IF        %subst(vindta:1:3) = 'H00'   AND
040300131121     C                             (%subst(vindta:7:3) = '101'  OR
040400131121     C                              %subst(vindta:7:3) = '833')
040500131120     C* ......VATNOT_B
040600131120     C                   EVAL      VATNOT=%trim(%subst(vindta:10:35))
040700131120     C                   EVAL      VATTRC = 'B'
040800131120     C                   IF        VATNOT <> *blanks
040900131120     C                   EXSR      VALVATCMR
041000131120     C                   WRITE     EDIVAT00
041100131120     C                   ENDIF
041200131120     C*
041300131120     C                   ENDIF
041400131120     C***
041500010202     C*
041600000801     C* Ebbene...
041700000801     C                   ADD       1             §CTRMO
041800010201     C                   IF        errore <> *zeros
041900000801     C                   ADD       1             §CTRNO
042000000801     C                   EVAL      recko = vindta
042100000801     C                   EVAL      vinflg = '2'
042200000801     C                   ELSE
042300010201     C                   ADD       1             §CTROKVB
042400000801     C                   ENDIF
042500000801     C*
042600000801     C                   ENDSR
042700070808     C*----------------------------------------------------*
042800070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAB
042900070808     C*----------------------------------------------------*
043000070808     C     VALVABCMR     BEGSR
043100070906     C*
043200070906     C                   EVAL      VABCMR = wCMR
043300070808     C                   EVAL      VABDCM = DATCOR
043400070808     C                   EVAL      VABDTS = DATCOR
043500070808     C                   EVAL      VABHMS = ORACOR
043600070808     C                   EVAL      VABCNT = 1
043700070808     C*
043800070808     C                   ENDSR
043900070808     C*----------------------------------------------------*
044000070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAT
044100070808     C*----------------------------------------------------*
044200070808     C     VALVATCMR     BEGSR
044300070808     C*
044400070808     C* VALORIZZO CAMPI RELATIVI AL "CMR"
044500070907     C                   EVAL      VATCMR = wCMR
044600070907     C                   EVAL      VATCNT = 1
044700070808     C*
044800070808     C                   ENDSR
044900010202     C*----------------------------------------------------*
045000070808     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
045100010202     C*----------------------------------------------------*
045200020305     C     PREVAT        BEGSR
045300010202     C*
045400070808     C* Compongo il nome del membro da dare al EDIVATWR
045500010202     C                   eval      parmbr = vlrhdl
045600010202     C                   movel     'M'           parmbr
045700060113     C                   eval      parccm = vlrksc
045800010202     C                   eval      paropz = '1'
045900010202     C* Effettuo la chiamata al CLLE preposto
046000070808     C                   call(e)   'TITVEVTC'
046100010202     C                   parm                    parccm
046200010202     C                   parm                    parmbr
046300010202     C                   parm                    paropz
046400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046500010202     C                   if        %error
046600010202     C                   movel     '1'           chkcall
046700010202     C                   else
046800010202     C                   movel     '0'           chkcall
046900010202     C                   endif
047000010202     C*
047100010202     C                   ENDSR
047200000801     C*----------------------------------------------------*
047300000801     C*  CONTROLLO NUMERICITA' CAMPI
047400000801     C*----------------------------------------------------*
047500000801     C     CHKNUM        BEGSR
047600000801     C*
047700000801     C                   call(e)   'ISNUMERIC'
047800000801     C                   PARM                    PiStr            30
047900040714     C                   PARM      ','           PiDecChr          1
048000000801     C                   PARM      *ZEROS        PiVal            30 9
048100000801     C                   PARM      '0'           PiInt             1
048200000801     C                   PARM      '0'           PiNum             1
048300000801     C                   IF        %error
048400000801     C                   EVAL      PiInt=*off
048500000801     C                   ENDIF
048600000801     C*
048700000801     C                   ENDSR
048800000801     C***
048900000801
049000130805
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
054500131105
054600131105
054700131105     C     WRIVAB        BEGSR
054800131105     C*
054900131105     C* NORMALIZZAIONE CAMPI NECESSARI "PULITI"
055000131105     C                   EXSR      exeBon
055100131105     C*
055200131105     C* VALORIZZO CAMPI RELATIVI AL "CMR"
055300131105     C                   WRITE     EDIVAB00
055400131105     C*
055500131105     C                   ENDSR
055600131105
055700131105
055800131105      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
055900131105     C     exeBon        BEGSR
056000131105     C*
056100131105     C* Imposto i parametri x il *pgm d "bonifica"
056200131105     C                   RESET                   ElencoChar
056300131105     C                   EVAL      TipoElenco = *blanks
056400131105     C                   EVAL      CharSost   = *blanks
056500131105     C                   EVAL      UpperCase  = '1'
056600131105     C                   EVAL      ChkNull    = '1'
056700131105     C                   EVAL      CharNull   = *blanks
056800131105     C*
056900131105     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
057000131105     C                   EVAL      TxtInOut   = VABLOD
057100131105     C                   EVAL      EsitoBon   = *blanks
057200131105     C*
057300131105     C                   CALL      'XCHKCHAR'
057400131105     C                   PARM                    TxtInOut
057500131105     C                   PARM                    ElencoChar
057600131105     C                   PARM                    TipoElenco
057700131105     C                   PARM                    CharSost
057800131105     C                   PARM                    UpperCase
057900131105     C                   PARM                    ChkNull
058000131105     C                   PARM                    CharNull
058100131105     C                   PARM                    EsitoBon
058200131105     C*
058300131105     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
058400131105     C                   IF        EsitoBon = '1'
058500131105     C                   EVAL      VABLOD = TxtInOut
058600131105     C                   ENDIF
058700131105     C*
058800131105     C* Elimino la provincia dalla località
058900131105     C                   movel     *blanks       wPrd              4
059000131105     C                   movel     *blanks       wLod             36
059100131105     C                   eval      wPrd = ' '+VABPRD+' '
059200131105     C                   eval      wLod = %trim(VABLOD)
059300131105     C                   z-add     1             wPos              2 0
059400131105     C                   dow       wPos <= %len(%trim(wLod))
059500131105     C                   if        %subst(wLod:wPos:4) = wPrd
059600131105     C                   eval      %subst(wLod:wPos:4) = *blanks
059700131105     C                   endif
059800131105     C                   eval      wPos = wPos + 1
059900131105     C                   enddo
060000131105     C                   eval      VABLOD = %trim(wLod)
060100131105     C*
060200131105     C*
060300131105     C* Compatto i *blanks di troppo
060400131105     C                   z-add     1             wPos              2 0
060500131105     C                   eval      wLod = %trim(VABLOD)
060600131105     C                   dow       wPos <= %len(%trim(wLod))
060700131105     C                   if        %subst(wLod:wPos:2) = *blanks
060800131105     C                   eval      wLod = %subst(wLod:1:wPos-1) +
060900131105     C                                    %subst(wLod:wPos+1)
061000131105     C                   eval      wPos = wPos - 1
061100131105     C                   endif
061200131105     C                   eval      wPos = wPos + 1
061300131105     C                   enddo
061400131105     C                   eval      VABLOD = wLod
061500131105     C*
061600131105     C                   ENDSR
061700011113
061800011113
061900000801
062000990920      /TITLE Invio dei dati al punto operativo.
062100010202     C     invio         BEGSR
062200990920     C*
062300070808     C* 1° invio EDIVAT
062400010201     C                   reset                   dscmz
062500010201     C                   move      vlrpoi        cmzdst
062600070808     C                   eval      cmzfld = 'EDIVATWR'
062700010201     C                   eval      cmzmbd = vlrhdl
062800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062900021009     C***                if        prmfir = *blanks
063000070808     C                   eval      cmzfla = 'EDIVAT0F'
063100070808     C                   eval      cmzmba = 'EDIVAT0F'
063200021009     C***                else
063300021009     C***                eval      cmzfla = prmfir
063400021009     C***                eval      cmzmba = prmfir
063500021009     C***                endif
063600010201     C                   eval      cmznrr = *zeros
063700020305     C                   move      §ctrokvt      cmznrr
063800021018     C                   eval      cmzlba = vlrfl1
063900010201     C                   call(e)   'TIS711C'
064000010201     C                   parm                    dscmz
064100010201     C                   parm      *blanks       esito
064200010205     C                   if        %error
064300010205     C                             or cmzerr = '1'
064400010205     C                             or esito  = '1'
064500010205     C                   eval      wrkesito = '3'
064600010205     C                   else
064700010201     C*
064800070808     C* 2° invio EDIVAB
064900010201     C                   reset                   dscmz
065000010201     C                   move      vlrpoi        cmzdst
065100010201     C                   eval      cmzfld = vlrfou
065200010201     C                   eval      cmzmbd = vlrhdl
065300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065400021009     C***                if        prmfir = *blanks
065500070808     C                   eval      cmzfla = 'EDIVAB0F'
065600070808     C                   eval      cmzmba = 'EDIVAB0F'
065700021009     C***                else
065800021009     C***                eval      cmzfla = prmfir
065900021009     C***                eval      cmzmba = prmfir
066000021009     C***                endif
066100010201     C                   eval      cmznrr = *zeros
066200010201     C                   move      §ctrokvb      cmznrr
066300021018     C                   eval      cmzlba = vlrfl1
066400010201     C                   call(e)   'TIS711C'
066500010201     C                   parm                    dscmz
066600010201     C                   parm      *blanks       esito
066700010201     C                   if        %error
066800010201     C                             or cmzerr = '1'
066900010201     C                             or esito  = '1'
067000010201     C                   eval      wrkesito = '3'
067100010201     C                   endif
067200010205     C                   endif
067300990920     C*
067400000613     C                   ENDSR
067500000613     C***
067600070411
067700130805
067800130805
067900070411     C     *pssr         BEGSR
068000070411     C*
068100070411     C                   if        %open(tivin00r)
068200070411     C                   close     tivin00r
068300070411     C                   endif
068400070808     C                   if        %open(edivabwr)
068500070808     C                   close     edivabwr
068600070411     C                   endif
068700070808     C                   if        %open(edivatwr)
068800070808     C                   close     edivatwr
068900070411     C                   endif
069000070411     C*
069100070411     C* Effettuo la chiamata al CLLE preposto
069200070808     C                   call(e)   'TITVEVTC'
069300070411     C                   parm                    parccm
069400070411     C                   parm                    parmbr
069500070411     C                   parm      '2'           paropz
069600070411     C*
069700070411     C                   eval      wrkesito = '2'
069800070411     C*
069900070411     C                   seton                                        LR
070000070411     C*
070100070411     C                   ENDSR     '*CANCL'
070200070411     C***
070300070411
070400130805
070500990910
070600000613     C     *inzsr        BEGSR
070700990910     C*
070800990910     C     *entry        plist
070900990920     C                   parm                    tivlrds
071000990921     C                   parm      wrkesito      esito
071100000724     C                   parm                    prmlit
071200000710     C                   parm                    prmfir
071300000613     C*
071400000830     C* CALCOLA LA DATA CORRENTE
071500000830     C                   time                    wn14             14 0
071600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071700070808     C                   movel     wn14          oracor            6 0          *ORA
071800130805     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071900130805     C                   eval      datcor = %dec(%date() : *ISO)
072000000830     C*
072100000613     C                   ENDSR
