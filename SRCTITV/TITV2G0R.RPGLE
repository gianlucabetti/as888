000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800990908
000900041210     D*------------
001000041210     D* SCHIERE
001100041210     D*------------
001200110610     D skNAZISO        S              3    DIM(1000)
001300041210     D skNAZBAR        S              3    DIM(1000)
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100110610     D ds15          e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700041210     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100050805     D FlgNewBol       s              1    INZ(*blanks)
003200110610     D jNAZ            s              5  0 INZ(*zeros)
003300000830
003400000830     D*------------------
003500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600000830     D*------------------
003700000830     D WLBDA8          DS                  INZ
003800000830     D  G08DAT                 1      8  0
003900000830     D  G08INV                 9     16  0
004000000830     D  G08ERR                17     17
004100000830     D  G08TGI                18     22  0
004200041025     D*------------------
004300041025     D* DS REPERIMENTO NUMERATORE
004400041025     D*------------------
004500050805     D trul33ds      e ds                  inz
004600041025     D*------------------
004700041025     D* DS ARCHITETTURA
004800041025     D*------------------
004900041025     D kpjba         e ds                  inz
005000041025     D*------------------
005100990908
005200010201
005300010201
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800041210     C                   EXSR      CARTAB                                       CARICA TABELLE
005900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006000000613     C*
006100010202     C* Effettuo la chiamata al CLLE preposto
006200040506     C                   call(e)   'TITVVTC'
006300010202     C                   parm                    parccm
006400010202     C                   parm                    parmbr
006500010202     C                   parm      '2'           paropz
006600000616     C*
006700010201     C                   seton                                        LR
006800041210
006900041210     C*--------------------------------------------------------
007000041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
007100041210     C*--------------------------------------------------------
007200110610     C     CARTAB        BEGSR
007300041210     C*
007400041210     C* TABELLA '15' - NAZIONI
007500041210     C                   clear                   skNAZISO
007600041210     C                   clear                   skNAZBAR
007700041210     C                   eval      tblKUT = 1
007800041210     C                   eval      tblCOD = '15'
007900041210     C     KEYtabP       setll     tabel00f
008000041210     C     KEYtabP       reade     tabel00f
008100041210     C                   dow       not %eof(tabel00f)
008200041210     C                   if        tblFLG = *blanks
008300041210     C                   movel(p)  tblUNI        ds15
008400041210     C                   add       1             jNAZ
008500041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
008600041210     C                   eval      skNAZISO(jNAZ) = §15COD
008700041210     C                   endif
008800041210     C     KEYtabP       reade     tabel00f
008900041210     C                   enddo
009000041210     C*
009100041210     C                   ENDSR
009200041210     C***
009300041210
009400041210
009500041210
009600910830     C*--------------------------------------------------------
009700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009800910830     C*--------------------------------------------------------
009900040526     C     RWFILE        BEGSR
010000990910     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400021113     C                   if        not %open(fivabwwr)
010500021113     C                   open      fivabwwr
010600990914     C                   endif
010700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010800020305     C                   exsr      prevat
010900010201     C*
011000010202     C                   if        chkcall = '0'
011100010202     C*
011200021113     C                   if        not %open(fivatwwr)
011300021113     C                   open      fivatwwr
011400010201     C                   endif
011500990910     C*
011600010201     C                   clear                   §CTROKVB          5 0
011700020305     C                   clear                   §CTROKVT          5 0
011800000801     C                   clear                   §CTRMO            5 0
011900000801     C                   clear                   §CTRNO            5 0
012000040910     C*
012100921023     C                   DO        *HIVAL
012200990913     C*
012300990915     C                   READ      tivin00r                               70
012400040910     C                   if        vindta > *blanks
012500000613     C                   add       1             rrnum
012600000801     C*
012700000801     C                   if        *in70 = *off
012800000801     C                             and
012900000801     C                             (vinflg = *blanks
013000000801     C                              or vinflg = '0'
013100000801     C                              or vinflg = '2')
013200000801     C*
013300000801     C                   clear                   vinmsg
013400000801     C                   eval      vinflg = '1'
013500040910     C*
013600040910     C* Eseguo routine d traduzione
013700040910     C                   exsr      impvabvat
013800040802     C*
013900010305     C                   endif
014000000905     C*
014100000905     C                   else
014200000905     C                   eval      vinflg = '1'
014300000905     C                   endif
014400000905     C*
014500000905     C  N70              update    tivin000
014600000905     C*
014700991022     C  N70              ENDdo
014800041210     C*
014900050805     C* Scarico i buffer eventualmente rimasti in sospeso
015000041210     C                   IF        FlgNewBol = '1'
015100110714     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
015200110714     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
015300041210     C                   WRITE     FIVAB000
015400041210     C                   ENDIF
015500010202     C*
015600010202     C                   endif
015700990910
015800990910     C* Se non ci sono record con errori ...
015900000710     C                   if        §ctrno = 0
016000990910     C* ... restituisco esito OK.
016100990921     C                   eval      wrkesito = '0'
016200990910     C                   else
016300010201     C                   if        §ctrokvb > 0
016400990921     C                   eval      wrkesito = '1'
016500000710     C                   else
016600000710     C                   eval      wrkesito = '2'
016700990910     C                   endif
016800000710     C                   endif
016900990910     C*
017000990914     C                   if        %open(tivin00r)
017100990908     C                   close     tivin00r
017200990914     C                   endif
017300021113     C                   if        %open(fivabwwr)
017400021113     C                   close     fivabwwr
017500990914     C                   endif
017600021113     C                   if        %open(fivatwwr)
017700021113     C                   close     fivatwwr
017800010201     C                   endif
017900990910     C*
018000010201     C                   if        §ctrokvb > 0
018100000724     C                             and vlrpoi <> *zeros
018200010202     C                   exsr      invio
018300990920     C                   endif
018400990920     C*
018500910830     C                   ENDSR
018600000613     C***
018700990920
018800000801     C*----------------------------------------------------*
018900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
019000000801     C*----------------------------------------------------*
019100010201     C     INZVAR        BEGSR
019200000801     C*
019300040802     C                   Z-ADD     *zeros        Num5_0            5 0
019400040802     C                   MOVEL     '0'           FlgCAS            1
019500110714     C                   MOVEL     *blanks       wNOTE            70
019600000801     C*
019700000801     C                   ENDSR
019800000801     C*----------------------------------------------------*
019900040910     C*  IMPOSTAZIONE CAMPI COSTANTI
020000000801     C*----------------------------------------------------*
020100000801     C     DEFCAM        BEGSR
020200000801     C*
020300021113     C                   CLEAR                   FIVAB000
020400040802     C                   CLEAR                   FIVAT000
020500020619     C* Imposto i valori di default...
020600110610     C                   Z-ADD     2221015       VABCCM
020700110610     C                   Z-ADD     2221015       VATCCM
020800110603     C                   Z-ADD     222           VABLNP
020900110603     C                   Z-ADD     222           VATLNP
021000110610     C                   Z-ADD     000           VABCTR
021100110610     C                   MOVEL     '1'           VABCBO
021200020619     C* ... e poi verifico se sono stati passati come parametri
021300020619     C                   IF        vlrppt > *blanks
021400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
021500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
021600020619     C                   EXSR      CHKNUM
021700020619     C                   IF        PiInt=*on
021800020619     C                   Z-ADD     PiVal         VABCCM
021900020619     C                   Z-ADD     PiVal         VATCCM
022000020619     C                   ENDIF
022100040506     C                   ENDIF
022200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
022300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
022400020619     C                   EXSR      CHKNUM
022500020619     C                   IF        PiInt=*on
022600020619     C                   Z-ADD     PiVal         VABLNP
022700020619     C                   Z-ADD     PiVal         VATLNP
022800040506     C                   ENDIF
022900020619     C                   ENDIF
023000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
023100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
023200020619     C                   EXSR      CHKNUM
023300020619     C                   IF        PiInt=*on
023400020619     C                   Z-ADD     PiVal         VABCTR
023500040506     C                   ENDIF
023600020619     C                   ENDIF
023700041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
023800041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
023900041210     C                   ENDIF
024000020619     C                   ENDIF
024100000801     C*
024200000801     C                   ENDSR
024300000801     C*----------------------------------------------------*
024400040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
024500000801     C*----------------------------------------------------*
024600040910     C     IMPVABVAT     BEGSR
024700041210     C*
024800041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
024900041210     C                   Z-ADD     *zeros        errore            1 0
025000040910     C*
025100041210     C********
025200050805     C* Tipo record 'KSTA_' (dati destinatario, importi e particolarità consegna)
025300041210     C********
025400050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KSTA_'
025500041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
025600041210     C                   IF        FlgNewBol = '1'
025700110714     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
025800110714     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
025900041210     C                   WRITE     FIVAB000
026000041210     C                   EVAL      FlgNewBol = '0'
026100041210     C                   ENDIF
026200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
026300040910     C                   EXSR      INZVAR
026400040910     C                   EXSR      DEFCAM
026500041210     C                   EVAL      FlgNewBol = '1'
026600040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
026700040928     C                   MOVEL     datcor        VABAAS
026800040928     C                   MOVEL     datcor        VATAAS
026900040928     C                   MOVE      datcor        VABMGS
027000040928     C                   MOVE(P)   vlrpoi        VABFGS
027100040928     C                   MOVE(P)   vlrpoi        VATFGS
027200041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
027300041210     C                   CLEAR                   TRUL33DS
027400041210     C                   EVAL      I33OPE = *zeros
027500041210     C                   EVAL      I33CNU = 302
027600041210     C                   EVAL      I33NUM = 1
027700041210     C                   MOVEL     TRUL33DS      KPJBU
027800041210     C                   CALL      'TRUL33R'
027900041210     C                   PARM                    KPJBA
028000041210     C                   MOVEL     KPJBU         TRUL33DS
028100041210     C                   IF        O33ERR = *zeros
028200041210     C                   Z-ADD     O33NRF        VABNSP
028300041210     C                   Z-ADD     O33NRF        VATNSP
028400041210     C                   ELSE
028500041210     C                   ADD       1             errore
028600041210     C                   EVAL      vinmsg = %trimr(vinmsg)
028700041025     C                             + ' ' + 'VABNSP VATNSP'
028800041210     C                   ENDIF
028900050805     C* ......VABRMA
029000110629     C                   EVAL      VABRMA=%trim(%subst(vindta:52:9))
029100110629     C* ......VABRMN
029200110629     C                   EVAL      PiStr=%trim(%subst(vindta:44:8))
029300110629     C                   EXSR      CHKNUM
029400110629     C                   IF        PiInt=*on
029500110629     C                   Z-ADD     PiVal         VABRMN
029600110629     C                   ELSE
029700110629     C                   Z-ADD     1             VABRMN
029800110629     C                   ADD       1             errore
029900110629     C                   EVAL      vinmsg = %trimr(vinmsg)
030000110629     C                             + ' ' + 'VABRMN'
030100110629     C                   ENDIF
030200050805     C* ......VABRSD/VABRD2
030300050805     C                   MOVEL     *blanks       wRAGSOCDEST      70
030400050805     C                   EVAL      wRAGSOCDEST=%trim(%subst(vindta:61:30))+' '+
030500050805     C                                         %trim(%subst(vindta:91:30))+' '+
030600050805     C                                         %trim(%subst(vindta:121:30))
030700050805     C                   EVAL      VABRSD=%trim(%subst(wRAGSOCDEST:1:35))
030800050805     C                   EVAL      VABRD2=%trim(%subst(wRAGSOCDEST:36:35))
030900050805     C* ......VABIND
031000110610     C                   EVAL      VABIND=%trim(%subst(vindta:151:30))
031100050805     C* ......VABNZD
031200110610     C                   EVAL      VABNZD=%trim(%subst(vindta:181:3))
031300110610     C                   Z-ADD     1             jNAZ
031400110610     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
031500110610     C                   IF        %found
031600110610     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
031700110610     C                   ENDIF
031800050805     C* ......VABLOD
031900110610     C                   EVAL      VABLOD=%trim(%subst(vindta:190:26))
032000110623     C* gestione particolare x provioncia inclusa nella località
032100110623     C                   IF        %subst(%trim(VABLOD):
032200110623     C                             %len(%trim(VABLOD))-2:1) = *blank
032300110623     C                   EVAL      VABPRD = %subst(%trim(VABLOD):
032400110623     C                                      %len(%trim(VABLOD))-1:2)
032500110623     C                   EVAL      VABLOD = %subst(%trim(VABLOD):1:
032600110623     C                                      %len(%trim(VABLOD))-2)
032700110623     C                   ENDIF
032800050805     C* ......VABCAD
032900050805     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
033000050805     C                   EXSR      CHKNUM
033100050805     C                   IF        PiInt=*on
033200050805     C                   Z-ADD     PiVal         Num5_0
033300050805     C                   MOVEL(P)  Num5_0        VABCAD
033400050805     C                   ELSE
033500050805     C                   ADD       1             errore
033600050805     C                   EVAL      vinmsg = %trimr(vinmsg)
033700050805     C                             + ' ' + 'VABCAD'
033800050805     C                   ENDIF
033900110603     C* ......VABCBO
034000110603     C                   IF        %trim(%subst(vindta:330:3)) = '011'
034100110603     C                   EVAL      VABCBO = '2'
034200110603     C                   ENDIF
034300050805     C* ......VABIAS
034400110808     C***                IF        %subst(vindta:334:9) <> *blanks AND
034500110808     C***                          %subst(vindta:334:9) <> *all'0'
034600110808     C***                EVAL      PiStr=%trim(%subst(vindta:334:9))
034700110808     C***                EXSR      CHKNUM
034800110808     C***                IF        PiNum=*on
034900110808     C***                EVAL      PiVal = PiVal / 100                          * gest 2 dec.
035000110808     C***                Z-ADD     PiVal         VABIAS
035100110808     C***                ELSE
035200110808     C***                ADD       1             errore
035300110808     C***                EVAL      vinmsg = %trimr(vinmsg)
035400110808     C***                          + ' ' + 'VABIAS'
035500110808     C***                ENDIF
035600110808     C***                IF        VABIAS > *zeros
035700110808     C***                EVAL      VABVAS = 'EUR'
035800110808     C***                IF        %trim(%subst(vindta:343:3)) = '001'
035900110808     C***                EVAL      VABVAS = 'CHF'
036000110808     C***                ENDIF
036100110808     C***                IF        %trim(%subst(vindta:343:3)) = '002'
036200110808     C***                EVAL      VABVAS = 'USD'
036300110808     C***                ENDIF
036400110808     C***                IF        %trim(%subst(vindta:343:3)) = '003'
036500110808     C***                EVAL      VABVAS = 'GBP'
036600110808     C***                ENDIF
036700110808     C***                IF        %trim(%subst(vindta:343:3)) = '006'
036800110808     C***                EVAL      VABVAS = 'NOK'
036900110808     C***                ENDIF
037000110808     C***                IF        %trim(%subst(vindta:343:3)) = '100'
037100110808     C***                EVAL      VABVAS = 'EUR'
037200110808     C***                ENDIF
037300110808     C***                IF        %trim(%subst(vindta:343:3)) = '114'
037400110808     C***                EVAL      VABVAS = '***'                               Fiorino Ungherese
037500110808     C***                ENDIF
037600110808     C***                ENDIF
037700110808     C***                ENDIF
037800050805     C* ......VABCAS
037900110603     C                   IF        %subst(vindta:368:9) <> *blanks AND
038000110603     C                             %subst(vindta:368:9) <> *all'0'
038100050805     C                   EVAL      PiStr=%trim(%subst(vindta:368:9))
038200050805     C                   EXSR      CHKNUM
038300050805     C                   IF        PiNum=*on
038400110603     C                   EVAL      PiVal = PiVal / 100                          * gest 2 dec.
038500050805     C                   Z-ADD     PiVal         VABCAS
038600050805     C                   MOVEL     '1'           FlgCAS
038700050805     C                   ELSE
038800050805     C                   ADD       1             errore
038900050805     C                   EVAL      vinmsg = %trimr(vinmsg)
039000050805     C                             + ' ' + 'VABCAS'
039100050805     C                   ENDIF
039200050805     C                   IF        VABCAS > *zeros
039300110603     C                   EVAL      VABVCA = 'EUR'
039400110603     C                   IF        %trim(%subst(vindta:377:3)) = '001'
039500050805     C                   EVAL      VABVCA = 'CHF'
039600050805     C                   ENDIF
039700110603     C                   IF        %trim(%subst(vindta:377:3)) = '002'
039800050805     C                   EVAL      VABVCA = 'USD'
039900050805     C                   ENDIF
040000110603     C                   IF        %trim(%subst(vindta:377:3)) = '003'
040100050805     C                   EVAL      VABVCA = 'GBP'
040200050805     C                   ENDIF
040300110603     C                   IF        %trim(%subst(vindta:377:3)) = '006'
040400050805     C                   EVAL      VABVCA = 'NOK'
040500050805     C                   ENDIF
040600110603     C                   IF        %trim(%subst(vindta:377:3)) = '100'
040700050805     C                   EVAL      VABVCA = 'EUR'
040800050805     C                   ENDIF
040900110603     C                   IF        %trim(%subst(vindta:377:3)) = '114'
041000050805     C                   EVAL      VABVCA = '***'                               Fiorino Ungherese
041100050805     C                   ENDIF
041200050805     C                   ENDIF
041300110603     C                   ENDIF
041400050805     C* ......VABDCR
041500110718     C***                IF        %trim(%subst(vindta:328:1)) = 'Q'
041600050805     C                   MOVEL     *blanks       wDCR              8
041700110603     C                   EVAL      wDCR=%subst(vindta:400:2)+
041800110603     C                                  %subst(vindta:398:2)+
041900050805     C                                  %subst(vindta:396:2)+
042000050805     C                                  %subst(vindta:394:2)
042100050805     C                   EVAL      PiStr=wDCR
042200050805     C                   EXSR      CHKNUM
042300050805     C                   IF        PiInt=*on
042400050805     C                   MOVEL     wDCR          VABDCR
042500050805     C                   ELSE
042600050805     C                   ADD       1             errore
042700050805     C                   EVAL      vinmsg = %trimr(vinmsg)
042800050805     C                             + ' ' + 'VABDCR'
042900050805     C                   ENDIF
043000110718     C***                ENDIF
043100050805     C*
043200050805     C                   ENDIF
043300050805     C*
043400050805     C********
043500050805     C* Tipo record 'KZEI_' (colli, peso e natura merce)
043600050805     C********
043700050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KZEI_'
043800050805     C* ......VABNCL
043900050805     C                   EVAL      PiStr=%trim(%subst(vindta:37:5))
044000050805     C                   EXSR      CHKNUM
044100050805     C                   IF        PiInt=*on
044200050805     C                   Z-ADD     PiVal         VABNCL
044300050805     C                   ELSE
044400050805     C                   ADD       1             errore
044500050805     C                   EVAL      vinmsg = %trimr(vinmsg)
044600050805     C                             + ' ' + 'VABNCL'
044700050805     C                   ENDIF
044800050805     C* ......VABNAS
044900050805     C                   EVAL      VABNAS=%trim(%subst(vindta:45:20))
045000050805     C* ......VABPKB
045100050805     C                   EVAL      PiStr=%trim(%subst(vindta:65:5))
045200050805     C                   EXSR      CHKNUM
045300050805     C                   IF        PiNum=*on
045400050805     C                   Z-ADD     PiVal         VABPKB
045500050805     C                   ELSE
045600050805     C                   ADD       1             errore
045700050805     C                   EVAL      vinmsg = %trimr(vinmsg)
045800050805     C                             + ' ' + 'VABPKB'
045900050805     C                   ENDIF
046000110603     C* ......VABVLB
046100110603     C                   IF        %subst(vindta:75:7) <> *blanks AND
046200110603     C                             %subst(vindta:75:7) <> *all'0'
046300110603     C                   EVAL      PiStr=%trim(%subst(vindta:75:7))
046400110603     C                   EXSR      CHKNUM
046500110603     C                   IF        PiNum=*on
046600110603     C                   EVAL      PiVal = PiVal / 1000                         * gest 3 dec
046700110603     C                   Z-ADD     PiVal         VABVLB
046800110603     C                   ELSE
046900110603     C                   ADD       1             errore
047000110603     C                   EVAL      vinmsg = %trimr(vinmsg)
047100110603     C                             + ' ' + 'VABVLB'
047200110603     C                   ENDIF
047300110603     C                   ENDIF
047400050805     C*
047500050805     C                   ENDIF
047600050805     C*
047700050805     C********
047800050805     C* Tipo record 'KPPA_' (barcode)
047900050805     C********
048000050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KPPA_'
048100050805     C* ......VATNOT/VATTRC
048200050805     C                   EVAL      VATTRC='E'
048300050805     C                   EVAL      VATNOT=%trim(%subst(vindta:39:35))
048400110616     C                   WRITE     FIVAT000
048500050805     C*
048600050805     C                   ENDIF
048700050805     C*
048800050805     C********
048900050805     C* Tipo record 'KUTX_' (riferimento mittente e note)
049000050805     C********
049100050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KUTX_'
049200110714     C* ......VABNOT/VABNT2
049300110714     C                   IF        %trim(%subst(vindta:37:2)) = 'AS'
049400110714     C                   EVAL      wNOTE = %trim(wNOTE) +
049500110714     C                                     %trim(%subst(vindta:39:60))
049600110714     C                   ENDIF
049700041210     C*
049800041210     C                   ENDIF
049900040910     C*
050000040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
050100040910     C                   IF        FlgCAS <> '0'
050200040929     C                   IF        VABCBO = '1'
050300040910     C                   EVAL      VABCBO = '4'
050400040910     C                   ELSE
050500040929     C                   EVAL      VABCBO = '6'
050600040910     C                   ENDIF
050700040929     C                   ENDIF
050800040910     C*
050900040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051000040910     C                   EXSR      CHKIMPDIV
051100010202     C*
051200000801     C* Ebbene...
051300000801     C                   ADD       1             §CTRMO
051400010201     C                   IF        errore <> *zeros
051500000801     C                   ADD       1             §CTRNO
051600000801     C                   EVAL      vinflg = '2'
051700000801     C                   ELSE
051800010201     C                   ADD       1             §CTROKVB
051900000801     C                   ENDIF
052000000801     C*
052100000801     C                   ENDSR
052200010202     C*----------------------------------------------------*
052300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052400010202     C*----------------------------------------------------*
052500020305     C     PREVAT        BEGSR
052600010202     C*
052700021113     C* Compongo il nome del membro da dare al FIVATWWR
052800010202     C                   eval      parmbr = vlrhdl
052900010202     C                   movel     'M'           parmbr
053000041210     C                   eval      parccm = vlrksc
053100010202     C                   eval      paropz = '1'
053200010202     C* Effettuo la chiamata al CLLE preposto
053300040506     C                   call(e)   'TITVVTC'
053400010202     C                   parm                    parccm
053500010202     C                   parm                    parmbr
053600010202     C                   parm                    paropz
053700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053800010202     C                   if        %error
053900010202     C                   movel     '1'           chkcall
054000010202     C                   else
054100010202     C                   movel     '0'           chkcall
054200010202     C                   endif
054300010202     C*
054400010202     C                   ENDSR
054500000801     C*----------------------------------------------------*
054600000801     C*  CONTROLLO NUMERICITA' CAMPI
054700000801     C*----------------------------------------------------*
054800000801     C     CHKNUM        BEGSR
054900000801     C*
055000000801     C                   call(e)   'ISNUMERIC'
055100000801     C                   PARM                    PiStr            30
055200041210     C                   PARM      '.'           PiDecChr          1
055300000801     C                   PARM      *ZEROS        PiVal            30 9
055400000801     C                   PARM      '0'           PiInt             1
055500000801     C                   PARM      '0'           PiNum             1
055600000801     C                   IF        %error
055700000801     C                   EVAL      PiInt=*off
055800000801     C                   ENDIF
055900000801     C*
056000000801     C                   ENDSR
056100000801     C***
056200000801
056300011113
056400011113     C*----------------------------------------------------*
056500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056600011113     C*----------------------------------------------------*
056700011113     C     CHKIMPDIV     BEGSR
056800011113     C*
056900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057000011113     C                   Z-ADD     *zeros        wrkDec            9 9
057100011113     C*
057200011113     C* Come prima cosa effettuo considerazioni sulla divisa
057300011113     C                   IF        vabIAS > *zeros
057400011113     C                   IF        vabVAS <> 'EUR'
057500011113     C                   EVAL      vabVAS =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C                   IF        vabCAS > *zeros
058000011113     C                   IF        vabVCA <> 'EUR'
058100011113     C                   EVAL      vabVCA =  'ITL'
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C                   IF        vabVMD > *zeros
058600020305     C                   IF        vabVAD <> 'EUR'
058700011113     C                   EVAL      vabVAD =  'ITL'
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059200011113     C                   Z-ADD     vabIAS        wrkDec
059300011113     C                   IF        wrkDec > *zeros
059400011113     C                   IF        vabVAS = 'ITL'
059500011113     C                   EVAL      vabIAS = *zeros
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060000011113     C                   Z-ADD     vabCAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVCA = 'ITL'
060300011113     C                   EVAL      vabCAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060800011113     C                   Z-ADD     vabVMD        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVAD = 'ITL'
061100011113     C                   EVAL      vabVMD = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C                   ENDSR
061600011113     C***
061700011113
061800011113
061900000801
062000000801
062100990920      /TITLE Invio dei dati al punto operativo.
062200010202     C     invio         BEGSR
062300990920     C*
062400021113     C* 1° invio FIVAT
062500010201     C                   reset                   dscmz
062600010201     C                   move      vlrpoi        cmzdst
062700021113     C                   eval      cmzfld = 'FIVATWWR'
062800010201     C                   eval      cmzmbd = vlrhdl
062900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063000021009     C***                if        prmfir = *blanks
063100021113     C                   eval      cmzfla = 'FIVAT00F'
063200021113     C                   eval      cmzmba = 'FIVAT00F'
063300021009     C***                else
063400021009     C***                eval      cmzfla = prmfir
063500021009     C***                eval      cmzmba = prmfir
063600021009     C***                endif
063700010201     C                   eval      cmznrr = *zeros
063800020305     C                   move      §ctrokvt      cmznrr
063900021018     C                   eval      cmzlba = vlrfl1
064000010201     C                   call(e)   'TIS711C'
064100010201     C                   parm                    dscmz
064200010201     C                   parm      *blanks       esito
064300010205     C                   if        %error
064400010205     C                             or cmzerr = '1'
064500010205     C                             or esito  = '1'
064600010205     C                   eval      wrkesito = '3'
064700010205     C                   else
064800010201     C*
064900021113     C* 2° invio FIVAB
065000010201     C                   reset                   dscmz
065100010201     C                   move      vlrpoi        cmzdst
065200010201     C                   eval      cmzfld = vlrfou
065300010201     C                   eval      cmzmbd = vlrhdl
065400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065500021009     C***                if        prmfir = *blanks
065600021113     C                   eval      cmzfla = 'FIVAB00F'
065700021113     C                   eval      cmzmba = 'FIVAB00F'
065800021009     C***                else
065900021009     C***                eval      cmzfla = prmfir
066000021009     C***                eval      cmzmba = prmfir
066100021009     C***                endif
066200010201     C                   eval      cmznrr = *zeros
066300010201     C                   move      §ctrokvb      cmznrr
066400021018     C                   eval      cmzlba = vlrfl1
066500010201     C                   call(e)   'TIS711C'
066600010201     C                   parm                    dscmz
066700010201     C                   parm      *blanks       esito
066800010201     C                   if        %error
066900010201     C                             or cmzerr = '1'
067000010201     C                             or esito  = '1'
067100010201     C                   eval      wrkesito = '3'
067200010201     C                   endif
067300010205     C                   endif
067400990920     C*
067500000613     C                   ENDSR
067600000613     C***
067700070411
067800070411     C     *pssr         BEGSR
067900070411     C*
068000070411     C                   if        %open(tivin00r)
068100070411     C                   close     tivin00r
068200070411     C                   endif
068300070411     C                   if        %open(fivabwwr)
068400070411     C                   close     fivabwwr
068500070411     C                   endif
068600070411     C                   if        %open(fivatwwr)
068700070411     C                   close     fivatwwr
068800070411     C                   endif
068900070411     C*
069000070411     C* Effettuo la chiamata al CLLE preposto
069100070411     C                   call(e)   'TITVVTC'
069200070411     C                   parm                    parccm
069300070411     C                   parm                    parmbr
069400070411     C                   parm      '2'           paropz
069500070411     C*
069600070411     C                   eval      wrkesito = '2'
069700070411     C*
069800070411     C                   seton                                        LR
069900070411     C*
070000070411     C                   ENDSR     '*CANCL'
070100070411     C***
070200070411
070300990910
070400000613     C     *inzsr        BEGSR
070500990910     C*
070600990910     C     *entry        plist
070700990920     C                   parm                    tivlrds
070800990921     C                   parm      wrkesito      esito
070900000724     C                   parm                    prmlit
071000000710     C                   parm                    prmfir
071100000613     C*
071200000830     C* CALCOLA LA DATA CORRENTE
071300000830     C                   time                    wn14             14 0
071400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
071500000830     C                   z-add     wn8           g08dat
071600000830     C                   z-add     *zeros        g08inv
071700000830     C                   movel     '0'           g08err
071800000830     C                   call      'XSRDA8'
071900000830     C                   parm                    wlbda8
072000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072100041210     C*
072200041210     C* Chiave su TABEL00F - parziale
072300041210     C     KEYtabP       KLIST
072400041210     C                   KFLD                    tblKUT
072500041210     C                   KFLD                    tblCOD
072600000830     C*
072700000613     C                   ENDSR
072800000613     C***
