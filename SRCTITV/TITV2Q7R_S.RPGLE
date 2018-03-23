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
004400010201
004500010201
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100000613     C*
005200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005300070719     C*
005400070719     C* Esegue lancio TISI95R solo x chiusura
005500070719     C                   CLEAR                   TISI95DS
005600070719     C                   EVAL      I95TLA = 'C'
005700070719     C                   CALL      'TISI95R'
005800070719     C                   PARM                    TISI95DS
005900000613     C*
006000010202     C* Effettuo la chiamata al CLLE preposto
006100070808     C                   call(e)   'TITVEVTC'
006200010202     C                   parm                    parccm
006300010202     C                   parm                    parmbr
006400010202     C                   parm      '2'           paropz
006500000616     C*
006600010201     C                   seton                                        LR
006700990908
006800000801
006900910830     C*--------------------------------------------------------
007000070808     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
007100910830     C*--------------------------------------------------------
007200040526     C     RWFILE        BEGSR
007300990910     C*
007400990914     C                   if        not %open(tivin00r)
007500990908     C                   open      tivin00r
007600990914     C                   endif
007700070808     C                   if        not %open(edivabwr)
007800070808     C                   open      edivabwr
007900990914     C                   endif
008000070808     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
008100020305     C                   exsr      prevat
008200010201     C*
008300010202     C                   if        chkcall = '0'
008400010202     C*
008500070808     C                   if        not %open(edivatwr)
008600070808     C                   open      edivatwr
008700010201     C                   endif
008800990910     C*
008900010201     C                   clear                   §CTROKVB          5 0
009000020305     C                   clear                   §CTROKVT          5 0
009100000801     C                   clear                   §CTRMO            5 0
009200000801     C                   clear                   §CTRNO            5 0
009300070919     C*
009400070919     C* Inizializzazioni iniziali e formati record file Bartolini
009500070919     C                   EXSR      INZVAR
009600070919     C                   EXSR      DEFCAM
009700040910     C*
009800921023     C                   DO        *HIVAL
009900990913     C*
010000990915     C                   READ      tivin00r                               70
010100040910     C                   if        vindta > *blanks
010200000613     C                   add       1             rrnum
010300000801     C*
010400000801     C                   if        *in70 = *off
010500000801     C                             and
010600000801     C                             (vinflg = *blanks
010700000801     C                              or vinflg = '0'
010800000801     C                              or vinflg = '2')
010900000801     C*
011000000801     C                   clear                   vinmsg
011100000801     C                   eval      vinflg = '1'
011200040910     C*
011300040910     C* Eseguo routine d traduzione
011400040910     C                   exsr      impvabvat
011500040802     C*
011600010305     C                   endif
011700000905     C*
011800000905     C                   else
011900000905     C                   eval      vinflg = '1'
012000000905     C                   endif
012100000905     C*
012200000905     C  N70              update    tivin000
012300000905     C*
012400991022     C  N70              ENDdo
012500010202     C*
012600070919     C* Scarico l'ultima bolla rimasta in canna
012700070919     C                   IF        wPrimaBolla = '1'
012800130805     C                   EXSR      VALVABCMR
012900070919     C                   WRITE     EDIVAB00
013000070919     C                   ENDIF
013100070919     C*
013200010202     C                   endif
013300990910
013400990910     C* Se non ci sono record con errori ...
013500000710     C                   if        §ctrno = 0
013600990910     C* ... restituisco esito OK.
013700990921     C                   eval      wrkesito = '0'
013800990910     C                   else
013900010201     C                   if        §ctrokvb > 0
014000990921     C                   eval      wrkesito = '1'
014100000710     C                   else
014200000710     C                   eval      wrkesito = '2'
014300990910     C                   endif
014400000710     C                   endif
014500990910     C*
014600990914     C                   if        %open(tivin00r)
014700990908     C                   close     tivin00r
014800990914     C                   endif
014900070808     C                   if        %open(edivabwr)
015000070808     C                   close     edivabwr
015100990914     C                   endif
015200070808     C                   if        %open(edivatwr)
015300070808     C                   close     edivatwr
015400010201     C                   endif
015500990910     C*
015600010201     C                   if        §ctrokvb > 0
015700000724     C                             and vlrpoi <> *zeros
015800010202     C                   exsr      invio
015900990920     C                   endif
016000990920     C*
016100910830     C                   ENDSR
016200000613     C***
016300990920
016400000801     C*----------------------------------------------------*
016500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016600000801     C*----------------------------------------------------*
016700010201     C     INZVAR        BEGSR
016800000801     C*
016900130805     C                   MOVEL     *blanks       wNOTE
017000040802     C                   Z-ADD     *zeros        Num5_0            5 0
017100040802     C                   MOVEL     '0'           FlgCAS            1
017200000801     C*
017300000801     C                   ENDSR
017400000801     C*----------------------------------------------------*
017500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017600000801     C*----------------------------------------------------*
017700000801     C     DEFCAM        BEGSR
017800000801     C*
017900070808     C                   CLEAR                   EDIVAB00
018000070808     C                   CLEAR                   EDIVAT00
018100020619     C* Imposto i valori di default...
018200130808     C                   Z-ADD     0436181       VABCCM
018300130808     C                   Z-ADD     0436181       VATCCM
018400130808     C                   Z-ADD     043           VABLNP
018500130808     C                   Z-ADD     043           VATLNP
018600130808     C                   Z-ADD     304           VABCTR
018700080825     C                   MOVEL     '7Q'          VABCTM
018800040823     C                   MOVEL     '1'           VABCBO
018900020619     C* ... e poi verifico se sono stati passati come parametri
019000020619     C                   IF        vlrppt > *blanks
019100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019300020619     C                   EXSR      CHKNUM
019400020619     C                   IF        PiInt=*on
019500020619     C                   Z-ADD     PiVal         VABCCM
019600020619     C                   Z-ADD     PiVal         VATCCM
019700020619     C                   ENDIF
019800040506     C                   ENDIF
019900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020100020619     C                   EXSR      CHKNUM
020200020619     C                   IF        PiInt=*on
020300020619     C                   Z-ADD     PiVal         VABLNP
020400020619     C                   Z-ADD     PiVal         VATLNP
020500040506     C                   ENDIF
020600020619     C                   ENDIF
020700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
020800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
020900020619     C                   EXSR      CHKNUM
021000020619     C                   IF        PiInt=*on
021100020619     C                   Z-ADD     PiVal         VABCTR
021200040506     C                   ENDIF
021300020619     C                   ENDIF
021400060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
021500060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
021600060202     C                   ENDIF
021700020619     C                   ENDIF
021800000801     C*
021900000801     C                   ENDSR
022000000801     C*----------------------------------------------------*
022100070808     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
022200000801     C*----------------------------------------------------*
022300130805     C     IMPVABVAT     BEGSR
022400131104     C*
022500131104     C* Forzatura iniziale: se presente coppia carattere ?' => '
022600131104     C     '?''':''' '   xlate     vindta        vindta
022700040910     C*
022800040910     C* Traduzione relativa ai tipi record del file del cliente
022900040910     C*
023000070719     C* Resetto indicatore d anomalia sul singolo record
023100070719     C                   eval      vinflg = '1'
023200130805     C***
023300130805     C* ...tipo record 'U01' (bordero)
023400130805     C                   IF        %subst(vindta:1:3) = 'U01'
023500130806     C                   eval      wCMR = %subst(vindta:73:6)
023600130805     C                   ENDIF
023700130805     C***
023800130805     C* ...tipo record 'U02' (bordero)
023900130805     C                   IF        %subst(vindta:1:3) = 'U02'
024000130805     C                   eval      wCMR = %trim(wCMR)+' '+%subst(vindta:4:14)
024100130805     C                   ENDIF
024200130805     C***
024300130805     C* ...tipo record 'U31' (testata)
024400130805     C                   IF        %subst(vindta:1:3) = 'U31'
024500130805     C* ......verifico se è la prima bolla elaborata
024600130805     C                   IF        wPrimaBolla = '0'
024700130805     C                   EVAL      wPrimaBolla = '1'
024800130805     C                   ELSE
024900130805     C* Considerazioni sul tipo porto
025000130805     C                   IF        FlgCAS <> '0'
025100130805     C                   IF        VABCBO = '1'
025200130805     C                   EVAL      VABCBO = '4'
025300130805     C                   ELSE
025400130805     C                   EVAL      VABCBO = '6'
025500130805     C                   ENDIF
025600130805     C                   ENDIF
025700130805     C* Eseguo routine finale x considerazioni specifiche su importi/divise
025800130805     C                   EXSR      CHKIMPDIV
025900130805     C* Considerazioni finali
026000130805     C                   EXSR      VALVABCMR
026100130805     C* Infine scarico il buffer di output
026200130805     C                   WRITE     EDIVAB00
026300130805     C* ......inizializzazioni iniziali e formati record file Bartolini
026400130805     C                   EXSR      INZVAR
026500130805     C                   EXSR      DEFCAM
026600130805     C                   ENDIF
026700130805     C*
026800130805     C                   Z-ADD     *zeros        errore            1 0
026900130805     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
027000130805     C                   MOVEL     datcor        VABAAS
027100130805     C                   MOVEL     datcor        VATAAS
027200130805     C                   MOVE      datcor        VABMGS
027300130805     C                   MOVE(P)   vlrpoi        VABFGS
027400130805     C                   MOVE(P)   vlrpoi        VATFGS
027500130805     C* ......VABNSP/VATNSP
027600130805     C* NSP => Stacco un numeratore da AZNUM
027700130805     C                   clear                   TRUL33DS
027800130805     C                   eval      I33OPE = *zeros
027900130805     C                   eval      I33CNU = 302
028000130805     C                   eval      I33NUM = 1
028100130805     C                   movel     TRUL33DS      KPJBU
028200130805     C                   call      'TRUL33R'
028300130805     C                   parm                    KPJBA
028400130805     C                   movel     KPJBU         TRUL33DS
028500130805     C                   if        O33ERR = *zeros
028600130805     C                   z-add     O33NRF        VABNSP
028700130805     C                   z-add     O33NRF        VATNSP
028800130805     C                   else
028900130805     C                   Z-ADD     1             errore
029000130805     C                   EVAL      vinmsg = %trimr(vinmsg)
029100130805     C                             + ' ' + 'VABNSP VATNSP'
029200130805     C                   endif
029300130805     C* ......VABNSP/VATNSP
029400130805     C*                  EVAL      PiStr=%trim(%subst(vindta:3:10))
029500130805     C*                  EXSR      CHKNUM
029600130805     C*                  IF        PiInt=*on
029700130805     C*                  Z-ADD     PiVal         VABNSP
029800130805     C*                  Z-ADD     PiVal         VATNSP
029900130805     C*                  ELSE
030000130805     C*                  ADD       1             errore
030100130805     C*                  EVAL      vinmsg = %trimr(vinmsg)
030200130805     C*                            + ' ' + 'VABNSP VATNSP'
030300130805     C*                  ENDIF
030400130805     C***
030500130805     C* ......VABRSD
030600130805     C                   EVAL      VABRSD=%trim(%subst(vindta:8:31))
030700130805     C* ......VABCAD
030800130805     C                   EVAL      VABCAD=%trim(%subst(vindta:39:5))
030900130805     C* ......VABIND
031000131024     C                   EVAL      VABIND=%trim(%subst(vindta:44:25))+' '+%trim(
031100131024     C                             (%editc(%dec(%subst(vindta:69:4):4:0):'4')))
031200130805     C* ......VABDCR
031300130805     C                   IF        %subst(vindta:73:8) <> *blanks
031400130805     C                   EVAL      PiStr=%trim(%subst(vindta:73:8))
031500060202     C                   EXSR      CHKNUM
031600060202     C                   IF        PiInt=*on
031700130805     C                   Z-ADD     PiVal         VABDCR
031800060202     C                   ELSE
031900060202     C                   ADD       1             errore
032000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
032100130805     C                             + ' ' + 'VABDCR'
032200060202     C                   ENDIF
032300130805     C                   ENDIF
032400130806     C* ......VATNOT "B" - Telefono destinatario
032500130806     C*                  EVAL      VATNOT=%trim(%subst(vindta:110:15))
032600130806     C*                  EVAL      VATTRC = 'B'
032700130806     C                   IF        VATNOT <> *blanks
032800130806     C                   EXSR      VALVATCMR
032900130806     C                   WRITE     EDIVAT00
033000130806     C                   ENDIF
033100130805     C*
033200130805     C                   ENDIF
033300130805     C***
033400130805     C* ...tipo record 'U32' (testata)
033500130805     C                   IF        %subst(vindta:1:3) = 'U32'
033600130805     C* ......VABLOD
033700130805     C                   EVAL      VABLOD=%trim(%subst(vindta:9:30))
033800130805     C* ......VABRD2
033900130805     C                   EVAL      VABRD2=%trim(%subst(vindta:39:36))
034000130805     C* ......VABNZD
034100130805     C                   EVAL      VABNZD=%trim(%subst(vindta:75:3))
034200131024     C                   IF        VABNZD = 'IT' OR VABNZD = 'I'
034300130805     C                   EVAL      VABNZD = *blanks
034400130805     C                   ENDIF
034500130805     C* ......VABPRD
034600130805     C* Reperisco la provincia dal CAP e dalla località
034700130805     C                   IF        VABNZD  = *blanks AND
034800130805     C                             VABPRD  = *blanks AND
034900130805     C                             VABCAD <> *blanks AND
035000130805     C                             VABLOD <> *blanks
035100130805     C                   CLEAR                   TISI95DS
035200130805     C                   EVAL      I95TCN = '3'
035300130805     C                   Z-ADD     datcor        I95DAT
035400130805     C                   EVAL      I95CAP = VABCAD
035500130805     C                   EVAL      I95LOC = VABLOD
035600130805     C                   CALL      'TISI95R'
035700130805     C                   PARM                    TISI95DS
035800130805     C                   EVAL      VABPRD = O95PRV
035900130805     C                   ENDIF
036000130805     C*
036100130805     C                   ENDIF
036200130805     C***
036300130805     C* ...tipo record 'U41' (testata)
036400130805     C                   IF        %subst(vindta:1:3) = 'U41'
036500130805     C* ......VABRMN
036600130805     C                   EVAL      PiStr=%trim(%subst(vindta:7:9))
036700130805     C                   EXSR      CHKNUM
036800130805     C                   IF        PiInt=*on
036900130805     C                   Z-ADD     PiVal         VABRMN
037000130805     C                   ELSE
037100130805     C                   ADD       1             errore
037200130805     C                   EVAL      vinmsg = %trimr(vinmsg)
037300130805     C                             + ' ' + 'VABRMN'
037400130805     C                   ENDIF
037500130805     C*
037600130805     C                   ENDIF
037700131105     C***
037800131105     C* ...tipo record 'U42' (testata - origin order nbr.)
037900131105     C                   IF        %subst(vindta:1:3) = 'U42'
038000131105     C* ......VABNAS
038100131105     C                   EVAL      VABNAS=%trim(%subst(vindta:4:50))
038200131105     C*
038300131105     C                   ENDIF
038400130805     C***
038500130805     C* ...tipo record 'U51' (testata - rif. RMA)
038600130805     C                   IF        %subst(vindta:1:3) = 'U51'
038700130805     C* ......VABRMA
038800130806     C                   EVAL      VABRMA=%trim(%subst(vindta:60:20))
038900130805     C*
039000130805     C                   ENDIF
039100130805     C***
039200130805     C* ...tipo record 'U52' (testata - note)
039300130805     C                   IF        %subst(vindta:1:3) = 'U52'
039400130805     C* ......VABNOT + VABNT2
039500130805     C                   EVAL      wNOTE=%trim(%subst(vindta:4:33))+
039600130805     C                                   %trim(%subst(vindta:38:33))
039700130805     C*
039800130805     C                   ENDIF
039900130805     C***
040000130805     C* ...tipo record 'U53' (testata - note)
040100130805     C                   IF        %subst(vindta:1:3) = 'U53'
040200130805     C* ......VABNOT + VABNT2
040300130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
040400130805     C                                   %trim(%subst(vindta:4:50))
040500130805     C*
040600130805     C                   ENDIF
040700130805     C***
040800130805     C* ...tipo record 'U54' (testata - note)
040900130805     C                   IF        %subst(vindta:1:3) = 'U54'
041000130805     C* ......VABNOT + VABNT2
041100130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
041200130805     C                                   %trim(%subst(vindta:4:77))
041300130805     C*
041400130805     C                   ENDIF
041500130805     C***
041600130805     C* ...tipo record 'U55' (testata - note)
041700130805     C                   IF        %subst(vindta:1:3) = 'U55'
041800130805     C* ......VABNOT + VABNT2
041900130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
042000130805     C                                   %trim(%subst(vindta:4:77))
042100130805     C*
042200130805     C                   ENDIF
042300130805     C***
042400130805     C* ...tipo record 'U56' (testata - note)
042500130805     C                   IF        %subst(vindta:1:3) = 'U56'
042600130805     C* ......VABNOT + VABNT2
042700130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
042800130805     C                                   %trim(%subst(vindta:4:77))
042900130805     C*
043000130805     C                   ENDIF
043100130805     C***
043200130805     C* ...tipo record 'U6' (testata - colli e peso)
043300130805     C                   IF        %subst(vindta:1:2) = 'U6'
043400130805     C* ......VABNCL (colli)
043500130805     C                   EVAL      PiStr=%trim(%subst(vindta:6:3))
043600130805     C                   EXSR      CHKNUM
043700130805     C                   IF        PiInt=*on
043800130912     C                   ADD       PiVal         VABNCL
043900130805     C                   ELSE
044000130805     C                   ADD       1             errore
044100130805     C                   EVAL      vinmsg = %trimr(vinmsg)
044200130805     C                             + ' ' + 'VABNCL'
044300130805     C                   ENDIF
044400130805     C* ......VABPKB
044500130805     C                   EVAL      PiStr=%trim(%subst(vindta:11:5))
044600130805     C                   EXSR      CHKNUM
044700130805     C                   IF        PiNum=*on
044800130912     C                   ADD       PiVal         VABPKB
044900130805     C                   ELSE
045000130805     C                   ADD       1             errore
045100130805     C                   EVAL      vinmsg = %trimr(vinmsg)
045200130805     C                             + ' ' + 'VABPKB'
045300130805     C                   ENDIF
045400130805     C* ......VABNCL (banacali)
045500130805     C*                  EVAL      PiStr=%trim(%subst(vindta:6:3))
045600130805     C*                  EXSR      CHKNUM
045700130805     C*                  IF        PiInt=*on
045800130912     C*                  ADD       PiVal         VABNCL
045900130805     C*                  ELSE
046000130805     C*                  ADD       1             errore
046100130805     C*                  EVAL      vinmsg = %trimr(vinmsg)
046200130805     C*                            + ' ' + 'VABNCL'
046300130805     C*                  ENDIF
046400130805     C*
046500130805     C                   ENDIF
046600130806     C***
046700130806     C* ...tipo record 'U8' (dettaglio segnacolli)
046800130806     C                   IF        %subst(vindta:1:2) = 'U8'
046900130806     C* ......VATNOT_E
047000130806     C                   EVAL      VATNOT=%trim(%subst(vindta:6:35))            * 20 long
047100130806     C                   EVAL      VATTRC = 'E'
047200130806     C                   IF        VATNOT <> *blanks
047300130806     C                   EXSR      VALVATCMR
047400130806     C                   WRITE     EDIVAT00
047500130806     C                   ENDIF
047600130806     C*
047700130806     C                   ENDIF
047800130805     C
047900010202     C*
048000000801     C* Ebbene...
048100000801     C                   ADD       1             §CTRMO
048200010201     C                   IF        errore <> *zeros
048300000801     C                   ADD       1             §CTRNO
048400000801     C                   EVAL      recko = vindta
048500000801     C                   EVAL      vinflg = '2'
048600000801     C                   ELSE
048700010201     C                   ADD       1             §CTROKVB
048800000801     C                   ENDIF
048900000801     C*
049000000801     C                   ENDSR
049100070808     C*----------------------------------------------------*
049200070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAB
049300070808     C*----------------------------------------------------*
049400070808     C     VALVABCMR     BEGSR
049500070906     C*
049600130805     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
049700130805     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
049800070906     C                   EVAL      VABCMR = wCMR
049900070808     C                   EVAL      VABDCM = DATCOR
050000070808     C                   EVAL      VABDTS = DATCOR
050100070808     C                   EVAL      VABHMS = ORACOR
050200070808     C                   EVAL      VABCNT = 1
050300070808     C*
050400070808     C                   ENDSR
050500070808     C*----------------------------------------------------*
050600070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAT
050700070808     C*----------------------------------------------------*
050800070808     C     VALVATCMR     BEGSR
050900070808     C*
051000070808     C* VALORIZZO CAMPI RELATIVI AL "CMR"
051100070907     C                   EVAL      VATCMR = wCMR
051200070907     C                   EVAL      VATCNT = 1
051300070808     C*
051400070808     C                   ENDSR
051500010202     C*----------------------------------------------------*
051600070808     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051700010202     C*----------------------------------------------------*
051800020305     C     PREVAT        BEGSR
051900010202     C*
052000070808     C* Compongo il nome del membro da dare al EDIVATWR
052100010202     C                   eval      parmbr = vlrhdl
052200010202     C                   movel     'M'           parmbr
052300060113     C                   eval      parccm = vlrksc
052400010202     C                   eval      paropz = '1'
052500010202     C* Effettuo la chiamata al CLLE preposto
052600070808     C                   call(e)   'TITVEVTC'
052700010202     C                   parm                    parccm
052800010202     C                   parm                    parmbr
052900010202     C                   parm                    paropz
053000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053100010202     C                   if        %error
053200010202     C                   movel     '1'           chkcall
053300010202     C                   else
053400010202     C                   movel     '0'           chkcall
053500010202     C                   endif
053600010202     C*
053700010202     C                   ENDSR
053800000801     C*----------------------------------------------------*
053900000801     C*  CONTROLLO NUMERICITA' CAMPI
054000000801     C*----------------------------------------------------*
054100000801     C     CHKNUM        BEGSR
054200000801     C*
054300000801     C                   call(e)   'ISNUMERIC'
054400000801     C                   PARM                    PiStr            30
054500040714     C                   PARM      ','           PiDecChr          1
054600000801     C                   PARM      *ZEROS        PiVal            30 9
054700000801     C                   PARM      '0'           PiInt             1
054800000801     C                   PARM      '0'           PiNum             1
054900000801     C                   IF        %error
055000000801     C                   EVAL      PiInt=*off
055100000801     C                   ENDIF
055200000801     C*
055300000801     C                   ENDSR
055400000801     C***
055500000801
055600130805
055700011113
055800011113     C*----------------------------------------------------*
055900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056000011113     C*----------------------------------------------------*
056100011113     C     CHKIMPDIV     BEGSR
056200011113     C*
056300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056400011113     C                   Z-ADD     *zeros        wrkDec            9 9
056500011113     C*
056600011113     C* Come prima cosa effettuo considerazioni sulla divisa
056700011113     C                   IF        vabIAS > *zeros
056800011113     C                   IF        vabVAS <> 'EUR'
056900011113     C                   EVAL      vabVAS =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C                   IF        vabCAS > *zeros
057400011113     C                   IF        vabVCA <> 'EUR'
057500011113     C                   EVAL      vabVCA =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C                   IF        vabVMD > *zeros
058000020305     C                   IF        vabVAD <> 'EUR'
058100011113     C                   EVAL      vabVAD =  'ITL'
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058600011113     C                   Z-ADD     vabIAS        wrkDec
058700011113     C                   IF        wrkDec > *zeros
058800011113     C                   IF        vabVAS = 'ITL'
058900011113     C                   EVAL      vabIAS = *zeros
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059400011113     C                   Z-ADD     vabCAS        wrkDec
059500011113     C                   IF        wrkDec > *zeros
059600011113     C                   IF        vabVCA = 'ITL'
059700011113     C                   EVAL      vabCAS = *zeros
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060200011113     C                   Z-ADD     vabVMD        wrkDec
060300011113     C                   IF        wrkDec > *zeros
060400011113     C                   IF        vabVAD = 'ITL'
060500011113     C                   EVAL      vabVMD = *zeros
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C                   ENDSR
061000011113     C***
061100011113
061200011113
061300000801
061400990920      /TITLE Invio dei dati al punto operativo.
061500010202     C     invio         BEGSR
061600990920     C*
061700070808     C* 1° invio EDIVAT
061800010201     C                   reset                   dscmz
061900010201     C                   move      vlrpoi        cmzdst
062000070808     C                   eval      cmzfld = 'EDIVATWR'
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400070808     C                   eval      cmzfla = 'EDIVAT0F'
062500070808     C                   eval      cmzmba = 'EDIVAT0F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100020305     C                   move      §ctrokvt      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010205     C                   if        %error
063700010205     C                             or cmzerr = '1'
063800010205     C                             or esito  = '1'
063900010205     C                   eval      wrkesito = '3'
064000010205     C                   else
064100010201     C*
064200070808     C* 2° invio EDIVAB
064300010201     C                   reset                   dscmz
064400010201     C                   move      vlrpoi        cmzdst
064500010201     C                   eval      cmzfld = vlrfou
064600010201     C                   eval      cmzmbd = vlrhdl
064700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064800021009     C***                if        prmfir = *blanks
064900070808     C                   eval      cmzfla = 'EDIVAB0F'
065000070808     C                   eval      cmzmba = 'EDIVAB0F'
065100021009     C***                else
065200021009     C***                eval      cmzfla = prmfir
065300021009     C***                eval      cmzmba = prmfir
065400021009     C***                endif
065500010201     C                   eval      cmznrr = *zeros
065600010201     C                   move      §ctrokvb      cmznrr
065700021018     C                   eval      cmzlba = vlrfl1
065800010201     C                   call(e)   'TIS711C'
065900010201     C                   parm                    dscmz
066000010201     C                   parm      *blanks       esito
066100010201     C                   if        %error
066200010201     C                             or cmzerr = '1'
066300010201     C                             or esito  = '1'
066400010201     C                   eval      wrkesito = '3'
066500010201     C                   endif
066600010205     C                   endif
066700990920     C*
066800000613     C                   ENDSR
066900000613     C***
067000070411
067100130805
067200130805
067300070411     C     *pssr         BEGSR
067400070411     C*
067500070411     C                   if        %open(tivin00r)
067600070411     C                   close     tivin00r
067700070411     C                   endif
067800070808     C                   if        %open(edivabwr)
067900070808     C                   close     edivabwr
068000070411     C                   endif
068100070808     C                   if        %open(edivatwr)
068200070808     C                   close     edivatwr
068300070411     C                   endif
068400070411     C*
068500070411     C* Effettuo la chiamata al CLLE preposto
068600070808     C                   call(e)   'TITVEVTC'
068700070411     C                   parm                    parccm
068800070411     C                   parm                    parmbr
068900070411     C                   parm      '2'           paropz
069000070411     C*
069100070411     C                   eval      wrkesito = '2'
069200070411     C*
069300070411     C                   seton                                        LR
069400070411     C*
069500070411     C                   ENDSR     '*CANCL'
069600070411     C***
069700070411
069800130805
069900990910
070000000613     C     *inzsr        BEGSR
070100990910     C*
070200990910     C     *entry        plist
070300990920     C                   parm                    tivlrds
070400990921     C                   parm      wrkesito      esito
070500000724     C                   parm                    prmlit
070600000710     C                   parm                    prmfir
070700000613     C*
070800000830     C* CALCOLA LA DATA CORRENTE
070900000830     C                   time                    wn14             14 0
071000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071100070808     C                   movel     wn14          oracor            6 0          *ORA
071200130805     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071300130805     C                   eval      datcor = %dec(%date() : *ISO)
071400000830     C*
071500000613     C                   ENDSR
