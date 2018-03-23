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
019200130808     C                   Z-ADD     0436181       VABCCM
019300130808     C                   Z-ADD     0436181       VATCCM
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
024300130805     C* ...tipo record 'U01' (bordero)
024400130805     C                   IF        %subst(vindta:1:3) = 'U01'
024500130806     C                   eval      wCMR = %subst(vindta:73:6)
024600130805     C                   ENDIF
024700130805     C***
024800130805     C* ...tipo record 'U02' (bordero)
024900130805     C                   IF        %subst(vindta:1:3) = 'U02'
025000130805     C                   eval      wCMR = %trim(wCMR)+' '+%subst(vindta:4:14)
025100130805     C                   ENDIF
025200130805     C***
025300130805     C* ...tipo record 'U31' (testata)
025400130805     C                   IF        %subst(vindta:1:3) = 'U31'
025500130805     C* ......verifico se è la prima bolla elaborata
025600130805     C                   IF        wPrimaBolla = '0'
025700130805     C                   EVAL      wPrimaBolla = '1'
025800130805     C                   ELSE
025900130805     C* Considerazioni sul tipo porto
026000130805     C                   IF        FlgCAS <> '0'
026100130805     C                   IF        VABCBO = '1'
026200130805     C                   EVAL      VABCBO = '4'
026300130805     C                   ELSE
026400130805     C                   EVAL      VABCBO = '6'
026500130805     C                   ENDIF
026600130805     C                   ENDIF
026700130805     C* Eseguo routine finale x considerazioni specifiche su importi/divise
026800130805     C                   EXSR      CHKIMPDIV
026900130805     C* Considerazioni finali
027000130805     C                   EXSR      VALVABCMR
027100130805     C* Infine scarico il buffer di output
027200131105     C                   EXSR      WRIVAB
027300130805     C* ......inizializzazioni iniziali e formati record file Bartolini
027400130805     C                   EXSR      INZVAR
027500130805     C                   EXSR      DEFCAM
027600130805     C                   ENDIF
027700130805     C*
027800130805     C                   Z-ADD     *zeros        errore            1 0
027900130805     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
028000130805     C                   MOVEL     datcor        VABAAS
028100130805     C                   MOVEL     datcor        VATAAS
028200130805     C                   MOVE      datcor        VABMGS
028300130805     C                   MOVE(P)   vlrpoi        VABFGS
028400130805     C                   MOVE(P)   vlrpoi        VATFGS
028500130805     C* ......VABNSP/VATNSP
028600130805     C* NSP => Stacco un numeratore da AZNUM
028700130805     C                   clear                   TRUL33DS
028800130805     C                   eval      I33OPE = *zeros
028900130805     C                   eval      I33CNU = 302
029000130805     C                   eval      I33NUM = 1
029100130805     C                   movel     TRUL33DS      KPJBU
029200130805     C                   call      'TRUL33R'
029300130805     C                   parm                    KPJBA
029400130805     C                   movel     KPJBU         TRUL33DS
029500130805     C                   if        O33ERR = *zeros
029600130805     C                   z-add     O33NRF        VABNSP
029700130805     C                   z-add     O33NRF        VATNSP
029800130805     C                   else
029900130805     C                   Z-ADD     1             errore
030000130805     C                   EVAL      vinmsg = %trimr(vinmsg)
030100130805     C                             + ' ' + 'VABNSP VATNSP'
030200130805     C                   endif
030300130805     C* ......VABNSP/VATNSP
030400130805     C*                  EVAL      PiStr=%trim(%subst(vindta:3:10))
030500130805     C*                  EXSR      CHKNUM
030600130805     C*                  IF        PiInt=*on
030700130805     C*                  Z-ADD     PiVal         VABNSP
030800130805     C*                  Z-ADD     PiVal         VATNSP
030900130805     C*                  ELSE
031000130805     C*                  ADD       1             errore
031100130805     C*                  EVAL      vinmsg = %trimr(vinmsg)
031200130805     C*                            + ' ' + 'VABNSP VATNSP'
031300130805     C*                  ENDIF
031400130805     C***
031500130805     C* ......VABRSD
031600130805     C                   EVAL      VABRSD=%trim(%subst(vindta:8:31))
031700130805     C* ......VABCAD
031800130805     C                   EVAL      VABCAD=%trim(%subst(vindta:39:5))
031900130805     C* ......VABIND
032000131024     C                   EVAL      VABIND=%trim(%subst(vindta:44:25))+' '+%trim(
032100131024     C                             (%editc(%dec(%subst(vindta:69:4):4:0):'4')))
032200130805     C* ......VABDCR
032300130805     C                   IF        %subst(vindta:73:8) <> *blanks
032400130805     C                   EVAL      PiStr=%trim(%subst(vindta:73:8))
032500060202     C                   EXSR      CHKNUM
032600060202     C                   IF        PiInt=*on
032700130805     C                   Z-ADD     PiVal         VABDCR
032800060202     C                   ELSE
032900060202     C                   ADD       1             errore
033000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
033100130805     C                             + ' ' + 'VABDCR'
033200060202     C                   ENDIF
033300130805     C                   ENDIF
033400130806     C* ......VATNOT "B" - Telefono destinatario
033500130806     C*                  EVAL      VATNOT=%trim(%subst(vindta:110:15))
033600130806     C*                  EVAL      VATTRC = 'B'
033700130806     C                   IF        VATNOT <> *blanks
033800130806     C                   EXSR      VALVATCMR
033900130806     C                   WRITE     EDIVAT00
034000130806     C                   ENDIF
034100130805     C*
034200130805     C                   ENDIF
034300130805     C***
034400130805     C* ...tipo record 'U32' (testata)
034500130805     C                   IF        %subst(vindta:1:3) = 'U32'
034600130805     C* ......VABLOD
034700130805     C                   EVAL      VABLOD=%trim(%subst(vindta:9:30))
034800130805     C* ......VABRD2
034900130805     C                   EVAL      VABRD2=%trim(%subst(vindta:39:36))
035000130805     C* ......VABNZD
035100130805     C                   EVAL      VABNZD=%trim(%subst(vindta:75:3))
035200131024     C                   IF        VABNZD = 'IT' OR VABNZD = 'I'
035300130805     C                   EVAL      VABNZD = *blanks
035400130805     C                   ENDIF
035500130805     C* ......VABPRD
035600130805     C* Reperisco la provincia dal CAP e dalla località
035700130805     C                   IF        VABNZD  = *blanks AND
035800130805     C                             VABPRD  = *blanks AND
035900130805     C                             VABCAD <> *blanks AND
036000130805     C                             VABLOD <> *blanks
036100130805     C                   CLEAR                   TISI95DS
036200130805     C                   EVAL      I95TCN = '3'
036300130805     C                   Z-ADD     datcor        I95DAT
036400130805     C                   EVAL      I95CAP = VABCAD
036500130805     C                   EVAL      I95LOC = VABLOD
036600130805     C                   CALL      'TISI95R'
036700130805     C                   PARM                    TISI95DS
036800130805     C                   EVAL      VABPRD = O95PRV
036900130805     C                   ENDIF
037000130805     C*
037100130805     C                   ENDIF
037200130805     C***
037300130805     C* ...tipo record 'U41' (testata)
037400130805     C                   IF        %subst(vindta:1:3) = 'U41'
037500130805     C* ......VABRMN
037600130805     C                   EVAL      PiStr=%trim(%subst(vindta:7:9))
037700130805     C                   EXSR      CHKNUM
037800130805     C                   IF        PiInt=*on
037900130805     C                   Z-ADD     PiVal         VABRMN
038000130805     C                   ELSE
038100130805     C                   ADD       1             errore
038200130805     C                   EVAL      vinmsg = %trimr(vinmsg)
038300130805     C                             + ' ' + 'VABRMN'
038400130805     C                   ENDIF
038500130805     C*
038600130805     C                   ENDIF
038700131105     C***
038800131105     C* ...tipo record 'U42' (testata - origin order nbr.)
038900131105     C                   IF        %subst(vindta:1:3) = 'U42'
039000131105     C* ......VABNAS
039100131105     C                   EVAL      VABNAS=%trim(%subst(vindta:4:50))
039200131105     C*
039300131105     C                   ENDIF
039400130805     C***
039500130805     C* ...tipo record 'U51' (testata - rif. RMA)
039600130805     C                   IF        %subst(vindta:1:3) = 'U51'
039700130805     C* ......VABRMA
039800130806     C                   EVAL      VABRMA=%trim(%subst(vindta:60:20))
039900130805     C*
040000130805     C                   ENDIF
040100130805     C***
040200130805     C* ...tipo record 'U52' (testata - note)
040300130805     C                   IF        %subst(vindta:1:3) = 'U52'
040400130805     C* ......VABNOT + VABNT2
040500130805     C                   EVAL      wNOTE=%trim(%subst(vindta:4:33))+
040600130805     C                                   %trim(%subst(vindta:38:33))
040700130805     C*
040800130805     C                   ENDIF
040900130805     C***
041000130805     C* ...tipo record 'U53' (testata - note)
041100130805     C                   IF        %subst(vindta:1:3) = 'U53'
041200130805     C* ......VABNOT + VABNT2
041300130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
041400130805     C                                   %trim(%subst(vindta:4:50))
041500130805     C*
041600130805     C                   ENDIF
041700130805     C***
041800130805     C* ...tipo record 'U54' (testata - note)
041900130805     C                   IF        %subst(vindta:1:3) = 'U54'
042000130805     C* ......VABNOT + VABNT2
042100130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
042200130805     C                                   %trim(%subst(vindta:4:77))
042300130805     C*
042400130805     C                   ENDIF
042500130805     C***
042600130805     C* ...tipo record 'U55' (testata - note)
042700130805     C                   IF        %subst(vindta:1:3) = 'U55'
042800130805     C* ......VABNOT + VABNT2
042900130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
043000130805     C                                   %trim(%subst(vindta:4:77))
043100130805     C*
043200130805     C                   ENDIF
043300130805     C***
043400130805     C* ...tipo record 'U56' (testata - note)
043500130805     C                   IF        %subst(vindta:1:3) = 'U56'
043600130805     C* ......VABNOT + VABNT2
043700130805     C                   EVAL      wNOTE=%trim(wNOTE)+' '+
043800130805     C                                   %trim(%subst(vindta:4:77))
043900130805     C*
044000130805     C                   ENDIF
044100130805     C***
044200130805     C* ...tipo record 'U6' (testata - colli e peso)
044300130805     C                   IF        %subst(vindta:1:2) = 'U6'
044400130805     C* ......VABNCL (colli)
044500130805     C                   EVAL      PiStr=%trim(%subst(vindta:6:3))
044600130805     C                   EXSR      CHKNUM
044700130805     C                   IF        PiInt=*on
044800130912     C                   ADD       PiVal         VABNCL
044900130805     C                   ELSE
045000130805     C                   ADD       1             errore
045100130805     C                   EVAL      vinmsg = %trimr(vinmsg)
045200130805     C                             + ' ' + 'VABNCL'
045300130805     C                   ENDIF
045400130805     C* ......VABPKB
045500130805     C                   EVAL      PiStr=%trim(%subst(vindta:11:5))
045600130805     C                   EXSR      CHKNUM
045700130805     C                   IF        PiNum=*on
045800130912     C                   ADD       PiVal         VABPKB
045900130805     C                   ELSE
046000130805     C                   ADD       1             errore
046100130805     C                   EVAL      vinmsg = %trimr(vinmsg)
046200130805     C                             + ' ' + 'VABPKB'
046300130805     C                   ENDIF
046400130805     C* ......VABNCL (banacali)
046500130805     C*                  EVAL      PiStr=%trim(%subst(vindta:6:3))
046600130805     C*                  EXSR      CHKNUM
046700130805     C*                  IF        PiInt=*on
046800130912     C*                  ADD       PiVal         VABNCL
046900130805     C*                  ELSE
047000130805     C*                  ADD       1             errore
047100130805     C*                  EVAL      vinmsg = %trimr(vinmsg)
047200130805     C*                            + ' ' + 'VABNCL'
047300130805     C*                  ENDIF
047400130805     C*
047500130805     C                   ENDIF
047600130806     C***
047700130806     C* ...tipo record 'U8' (dettaglio segnacolli)
047800130806     C                   IF        %subst(vindta:1:2) = 'U8'
047900130806     C* ......VATNOT_E
048000130806     C                   EVAL      VATNOT=%trim(%subst(vindta:6:35))            * 20 long
048100130806     C                   EVAL      VATTRC = 'E'
048200130806     C                   IF        VATNOT <> *blanks
048300130806     C                   EXSR      VALVATCMR
048400130806     C                   WRITE     EDIVAT00
048500130806     C                   ENDIF
048600130806     C*
048700130806     C                   ENDIF
048800130805     C
048900010202     C*
049000000801     C* Ebbene...
049100000801     C                   ADD       1             §CTRMO
049200010201     C                   IF        errore <> *zeros
049300000801     C                   ADD       1             §CTRNO
049400000801     C                   EVAL      recko = vindta
049500000801     C                   EVAL      vinflg = '2'
049600000801     C                   ELSE
049700010201     C                   ADD       1             §CTROKVB
049800000801     C                   ENDIF
049900000801     C*
050000000801     C                   ENDSR
050100070808     C*----------------------------------------------------*
050200070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAB
050300070808     C*----------------------------------------------------*
050400070808     C     VALVABCMR     BEGSR
050500070906     C*
050600130805     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
050700130805     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
050800070906     C                   EVAL      VABCMR = wCMR
050900070808     C                   EVAL      VABDCM = DATCOR
051000070808     C                   EVAL      VABDTS = DATCOR
051100070808     C                   EVAL      VABHMS = ORACOR
051200070808     C                   EVAL      VABCNT = 1
051300070808     C*
051400070808     C                   ENDSR
051500070808     C*----------------------------------------------------*
051600070808     C*  VALORIZZAZIONE CAMPI "CMR" X VAT
051700070808     C*----------------------------------------------------*
051800070808     C     VALVATCMR     BEGSR
051900070808     C*
052000070808     C* VALORIZZO CAMPI RELATIVI AL "CMR"
052100070907     C                   EVAL      VATCMR = wCMR
052200070907     C                   EVAL      VATCNT = 1
052300070808     C*
052400070808     C                   ENDSR
052500010202     C*----------------------------------------------------*
052600070808     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
052700010202     C*----------------------------------------------------*
052800020305     C     PREVAT        BEGSR
052900010202     C*
053000070808     C* Compongo il nome del membro da dare al EDIVATWR
053100010202     C                   eval      parmbr = vlrhdl
053200010202     C                   movel     'M'           parmbr
053300060113     C                   eval      parccm = vlrksc
053400010202     C                   eval      paropz = '1'
053500010202     C* Effettuo la chiamata al CLLE preposto
053600070808     C                   call(e)   'TITVEVTC'
053700010202     C                   parm                    parccm
053800010202     C                   parm                    parmbr
053900010202     C                   parm                    paropz
054000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054100010202     C                   if        %error
054200010202     C                   movel     '1'           chkcall
054300010202     C                   else
054400010202     C                   movel     '0'           chkcall
054500010202     C                   endif
054600010202     C*
054700010202     C                   ENDSR
054800000801     C*----------------------------------------------------*
054900000801     C*  CONTROLLO NUMERICITA' CAMPI
055000000801     C*----------------------------------------------------*
055100000801     C     CHKNUM        BEGSR
055200000801     C*
055300000801     C                   call(e)   'ISNUMERIC'
055400000801     C                   PARM                    PiStr            30
055500040714     C                   PARM      ','           PiDecChr          1
055600000801     C                   PARM      *ZEROS        PiVal            30 9
055700000801     C                   PARM      '0'           PiInt             1
055800000801     C                   PARM      '0'           PiNum             1
055900000801     C                   IF        %error
056000000801     C                   EVAL      PiInt=*off
056100000801     C                   ENDIF
056200000801     C*
056300000801     C                   ENDSR
056400000801     C***
056500000801
056600130805
056700011113
056800011113     C*----------------------------------------------------*
056900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057000011113     C*----------------------------------------------------*
057100011113     C     CHKIMPDIV     BEGSR
057200011113     C*
057300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057400011113     C                   Z-ADD     *zeros        wrkDec            9 9
057500011113     C*
057600011113     C* Come prima cosa effettuo considerazioni sulla divisa
057700011113     C                   IF        vabIAS > *zeros
057800011113     C                   IF        vabVAS <> 'EUR'
057900011113     C                   EVAL      vabVAS =  'ITL'
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   IF        vabCAS > *zeros
058400011113     C                   IF        vabVCA <> 'EUR'
058500011113     C                   EVAL      vabVCA =  'ITL'
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   IF        vabVMD > *zeros
059000020305     C                   IF        vabVAD <> 'EUR'
059100011113     C                   EVAL      vabVAD =  'ITL'
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059600011113     C                   Z-ADD     vabIAS        wrkDec
059700011113     C                   IF        wrkDec > *zeros
059800011113     C                   IF        vabVAS = 'ITL'
059900011113     C                   EVAL      vabIAS = *zeros
060000011113     C                   ENDIF
060100011113     C                   ENDIF
060200011113     C*
060300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060400011113     C                   Z-ADD     vabCAS        wrkDec
060500011113     C                   IF        wrkDec > *zeros
060600011113     C                   IF        vabVCA = 'ITL'
060700011113     C                   EVAL      vabCAS = *zeros
060800011113     C                   ENDIF
060900011113     C                   ENDIF
061000011113     C*
061100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061200011113     C                   Z-ADD     vabVMD        wrkDec
061300011113     C                   IF        wrkDec > *zeros
061400011113     C                   IF        vabVAD = 'ITL'
061500011113     C                   EVAL      vabVMD = *zeros
061600011113     C                   ENDIF
061700011113     C                   ENDIF
061800011113     C*
061900011113     C                   ENDSR
062000011113     C***
062100131105
062200131105
062300131105     C     WRIVAB        BEGSR
062400131105     C*
062500131105     C* NORMALIZZAIONE CAMPI NECESSARI "PULITI"
062600131105     C                   EXSR      exeBon
062700131105     C*
062800131105     C* VALORIZZO CAMPI RELATIVI AL "CMR"
062900131105     C                   WRITE     EDIVAB00
063000131105     C*
063100131105     C                   ENDSR
063200131105
063300131105
063400131105      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
063500131105     C     exeBon        BEGSR
063600131105     C*
063700131105     C* Imposto i parametri x il *pgm d "bonifica"
063800131105     C                   RESET                   ElencoChar
063900131105     C                   EVAL      TipoElenco = *blanks
064000131105     C                   EVAL      CharSost   = *blanks
064100131105     C                   EVAL      UpperCase  = '1'
064200131105     C                   EVAL      ChkNull    = '1'
064300131105     C                   EVAL      CharNull   = *blanks
064400131105     C*
064500131105     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
064600131105     C                   EVAL      TxtInOut   = VABLOD
064700131105     C                   EVAL      EsitoBon   = *blanks
064800131105     C*
064900131105     C                   CALL      'XCHKCHAR'
065000131105     C                   PARM                    TxtInOut
065100131105     C                   PARM                    ElencoChar
065200131105     C                   PARM                    TipoElenco
065300131105     C                   PARM                    CharSost
065400131105     C                   PARM                    UpperCase
065500131105     C                   PARM                    ChkNull
065600131105     C                   PARM                    CharNull
065700131105     C                   PARM                    EsitoBon
065800131105     C*
065900131105     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
066000131105     C                   IF        EsitoBon = '1'
066100131105     C                   EVAL      VABLOD = TxtInOut
066200131105     C                   ENDIF
066300131105     C*
066400131105     C* Elimino la provincia dalla località
066500131105     C                   movel     *blanks       wPrd              4
066600131105     C                   movel     *blanks       wLod             36
066700131105     C                   eval      wPrd = ' '+VABPRD+' '
066800131105     C                   eval      wLod = %trim(VABLOD)
066900131105     C                   z-add     1             wPos              2 0
067000131105     C                   dow       wPos <= %len(%trim(wLod))
067100131105     C                   if        %subst(wLod:wPos:4) = wPrd
067200131105     C                   eval      %subst(wLod:wPos:4) = *blanks
067300131105     C                   endif
067400131105     C                   eval      wPos = wPos + 1
067500131105     C                   enddo
067600131105     C                   eval      VABLOD = %trim(wLod)
067700131105     C*
067800131105     C*
067900131105     C* Compatto i *blanks di troppo
068000131105     C                   z-add     1             wPos              2 0
068100131105     C                   eval      wLod = %trim(VABLOD)
068200131105     C                   dow       wPos <= %len(%trim(wLod))
068300131105     C                   if        %subst(wLod:wPos:2) = *blanks
068400131105     C                   eval      wLod = %subst(wLod:1:wPos-1) +
068500131105     C                                    %subst(wLod:wPos+1)
068600131105     C                   eval      wPos = wPos - 1
068700131105     C                   endif
068800131105     C                   eval      wPos = wPos + 1
068900131105     C                   enddo
069000131105     C                   eval      VABLOD = wLod
069100131105     C*
069200131105     C                   ENDSR
069300011113
069400011113
069500000801
069600990920      /TITLE Invio dei dati al punto operativo.
069700010202     C     invio         BEGSR
069800990920     C*
069900070808     C* 1° invio EDIVAT
070000010201     C                   reset                   dscmz
070100010201     C                   move      vlrpoi        cmzdst
070200070808     C                   eval      cmzfld = 'EDIVATWR'
070300010201     C                   eval      cmzmbd = vlrhdl
070400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070500021009     C***                if        prmfir = *blanks
070600070808     C                   eval      cmzfla = 'EDIVAT0F'
070700070808     C                   eval      cmzmba = 'EDIVAT0F'
070800021009     C***                else
070900021009     C***                eval      cmzfla = prmfir
071000021009     C***                eval      cmzmba = prmfir
071100021009     C***                endif
071200010201     C                   eval      cmznrr = *zeros
071300020305     C                   move      §ctrokvt      cmznrr
071400021018     C                   eval      cmzlba = vlrfl1
071500010201     C                   call(e)   'TIS711C'
071600010201     C                   parm                    dscmz
071700010201     C                   parm      *blanks       esito
071800010205     C                   if        %error
071900010205     C                             or cmzerr = '1'
072000010205     C                             or esito  = '1'
072100010205     C                   eval      wrkesito = '3'
072200010205     C                   else
072300010201     C*
072400070808     C* 2° invio EDIVAB
072500010201     C                   reset                   dscmz
072600010201     C                   move      vlrpoi        cmzdst
072700010201     C                   eval      cmzfld = vlrfou
072800010201     C                   eval      cmzmbd = vlrhdl
072900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073000021009     C***                if        prmfir = *blanks
073100070808     C                   eval      cmzfla = 'EDIVAB0F'
073200070808     C                   eval      cmzmba = 'EDIVAB0F'
073300021009     C***                else
073400021009     C***                eval      cmzfla = prmfir
073500021009     C***                eval      cmzmba = prmfir
073600021009     C***                endif
073700010201     C                   eval      cmznrr = *zeros
073800010201     C                   move      §ctrokvb      cmznrr
073900021018     C                   eval      cmzlba = vlrfl1
074000010201     C                   call(e)   'TIS711C'
074100010201     C                   parm                    dscmz
074200010201     C                   parm      *blanks       esito
074300010201     C                   if        %error
074400010201     C                             or cmzerr = '1'
074500010201     C                             or esito  = '1'
074600010201     C                   eval      wrkesito = '3'
074700010201     C                   endif
074800010205     C                   endif
074900990920     C*
075000000613     C                   ENDSR
075100000613     C***
075200070411
075300130805
075400130805
075500070411     C     *pssr         BEGSR
075600070411     C*
075700070411     C                   if        %open(tivin00r)
075800070411     C                   close     tivin00r
075900070411     C                   endif
076000070808     C                   if        %open(edivabwr)
076100070808     C                   close     edivabwr
076200070411     C                   endif
076300070808     C                   if        %open(edivatwr)
076400070808     C                   close     edivatwr
076500070411     C                   endif
076600070411     C*
076700070411     C* Effettuo la chiamata al CLLE preposto
076800070808     C                   call(e)   'TITVEVTC'
076900070411     C                   parm                    parccm
077000070411     C                   parm                    parmbr
077100070411     C                   parm      '2'           paropz
077200070411     C*
077300070411     C                   eval      wrkesito = '2'
077400070411     C*
077500070411     C                   seton                                        LR
077600070411     C*
077700070411     C                   ENDSR     '*CANCL'
077800070411     C***
077900070411
078000130805
078100990910
078200000613     C     *inzsr        BEGSR
078300990910     C*
078400990910     C     *entry        plist
078500990920     C                   parm                    tivlrds
078600990921     C                   parm      wrkesito      esito
078700000724     C                   parm                    prmlit
078800000710     C                   parm                    prmfir
078900000613     C*
079000000830     C* CALCOLA LA DATA CORRENTE
079100000830     C                   time                    wn14             14 0
079200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
079300070808     C                   movel     wn14          oracor            6 0          *ORA
079400130805     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079500130805     C                   eval      datcor = %dec(%date() : *ISO)
079600000830     C*
079700000613     C                   ENDSR
