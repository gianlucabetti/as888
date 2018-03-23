000100150427      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400041210     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600150427     FEDIVABwr  O    E             DISK    usropn
000700150427     FEDIVATwr  O    E             DISK    usropn
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
003300150427     D wNomeFile       s             30    INZ(*blanks)
003400000830
003500000830     D*------------------
003600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003700000830     D*------------------
003800000830     D WLBDA8          DS                  INZ
003900000830     D  G08DAT                 1      8  0
004000000830     D  G08INV                 9     16  0
004100000830     D  G08ERR                17     17
004200000830     D  G08TGI                18     22  0
004300041025     D*------------------
004400041025     D* DS REPERIMENTO NUMERATORE
004500041025     D*------------------
004600050805     D trul33ds      e ds                  inz
004700041025     D*------------------
004800041025     D* DS ARCHITETTURA
004900041025     D*------------------
005000041025     D kpjba         e ds                  inz
005100041025     D*------------------
005200990908
005300010201
005400010201
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800000613     C*
005900041210     C                   EXSR      CARTAB                                       CARICA TABELLE
006000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006100000613     C*
006200010202     C* Effettuo la chiamata al CLLE preposto
006300150427     C                   call(e)   'TITVEVTC'
006400010202     C                   parm                    parccm
006500010202     C                   parm                    parmbr
006600010202     C                   parm      '2'           paropz
006700000616     C*
006800010201     C                   seton                                        LR
006900041210
007000041210     C*--------------------------------------------------------
007100041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
007200041210     C*--------------------------------------------------------
007300110610     C     CARTAB        BEGSR
007400041210     C*
007500041210     C* TABELLA '15' - NAZIONI
007600041210     C                   clear                   skNAZISO
007700041210     C                   clear                   skNAZBAR
007800041210     C                   eval      tblKUT = 1
007900041210     C                   eval      tblCOD = '15'
008000041210     C     KEYtabP       setll     tabel00f
008100041210     C     KEYtabP       reade     tabel00f
008200041210     C                   dow       not %eof(tabel00f)
008300041210     C                   if        tblFLG = *blanks
008400041210     C                   movel(p)  tblUNI        ds15
008500041210     C                   add       1             jNAZ
008600041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
008700041210     C                   eval      skNAZISO(jNAZ) = §15COD
008800041210     C                   endif
008900041210     C     KEYtabP       reade     tabel00f
009000041210     C                   enddo
009100041210     C*
009200041210     C                   ENDSR
009300041210     C***
009400041210
009500041210
009600041210
009700910830     C*--------------------------------------------------------
009800150427     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
009900910830     C*--------------------------------------------------------
010000040526     C     RWFILE        BEGSR
010100990910     C*
010200990914     C                   if        not %open(tivin00r)
010300990908     C                   open      tivin00r
010400990914     C                   endif
010500150427     C                   if        not %open(edivabwr)
010600150427     C                   open      edivabwr
010700990914     C                   endif
010800150427     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
010900020305     C                   exsr      prevat
011000010201     C*
011100010202     C                   if        chkcall = '0'
011200010202     C*
011300150427     C                   if        not %open(edivatwr)
011400150427     C                   open      edivatwr
011500010201     C                   endif
011600990910     C*
011700010201     C                   clear                   §CTROKVB          5 0
011800020305     C                   clear                   §CTROKVT          5 0
011900000801     C                   clear                   §CTRMO            5 0
012000000801     C                   clear                   §CTRNO            5 0
012100040910     C*
012200921023     C                   DO        *HIVAL
012300990913     C*
012400990915     C                   READ      tivin00r                               70
012500040910     C                   if        vindta > *blanks
012600000613     C                   add       1             rrnum
012700000801     C*
012800000801     C                   if        *in70 = *off
012900000801     C                             and
013000000801     C                             (vinflg = *blanks
013100000801     C                              or vinflg = '0'
013200000801     C                              or vinflg = '2')
013300000801     C*
013400000801     C                   clear                   vinmsg
013500000801     C                   eval      vinflg = '1'
013600040910     C*
013700040910     C* Eseguo routine d traduzione
013800040910     C                   exsr      impvabvat
013900040802     C*
014000010305     C                   endif
014100000905     C*
014200000905     C                   else
014300000905     C                   eval      vinflg = '1'
014400000905     C                   endif
014500000905     C*
014600000905     C  N70              update    tivin000
014700000905     C*
014800991022     C  N70              ENDdo
014900041210     C*
015000050805     C* Scarico i buffer eventualmente rimasti in sospeso
015100041210     C                   IF        FlgNewBol = '1'
015200110714     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
015300110714     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
015400150427     C*
015500150427     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
015600150427     C                   MOVE(P)   vlrMSG        wNomeFile
015700150427     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor) +
015800150427     C                                    ' CMR:' +
015900150427     C                                    %subst(wNomeFile:6:3)
016000150427     C                   EVAL      VABDCM = datcor
016100150427     C                   EVAL      VABDTS = datcor
016200150427     C                   EVAL      VABHMS = oracor
016300150427     C                   EVAL      VABCNT = 1
016400150427     C*
016500150427     C                   WRITE     EDIVAB00
016600041210     C                   ENDIF
016700010202     C*
016800010202     C                   endif
016900990910
017000990910     C* Se non ci sono record con errori ...
017100000710     C                   if        §ctrno = 0
017200990910     C* ... restituisco esito OK.
017300990921     C                   eval      wrkesito = '0'
017400990910     C                   else
017500010201     C                   if        §ctrokvb > 0
017600990921     C                   eval      wrkesito = '1'
017700000710     C                   else
017800000710     C                   eval      wrkesito = '2'
017900990910     C                   endif
018000000710     C                   endif
018100990910     C*
018200990914     C                   if        %open(tivin00r)
018300990908     C                   close     tivin00r
018400990914     C                   endif
018500150427     C                   if        %open(edivabwr)
018600150427     C                   close     edivabwr
018700990914     C                   endif
018800150427     C                   if        %open(edivatwr)
018900150427     C                   close     edivatwr
019000010201     C                   endif
019100990910     C*
019200010201     C                   if        §ctrokvb > 0
019300000724     C                             and vlrpoi <> *zeros
019400010202     C                   exsr      invio
019500990920     C                   endif
019600990920     C*
019700910830     C                   ENDSR
019800000613     C***
019900990920
020000000801     C*----------------------------------------------------*
020100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020200000801     C*----------------------------------------------------*
020300010201     C     INZVAR        BEGSR
020400000801     C*
020500040802     C                   Z-ADD     *zeros        Num5_0            5 0
020600040802     C                   MOVEL     '0'           FlgCAS            1
020700110714     C                   MOVEL     *blanks       wNOTE            70
020800000801     C*
020900000801     C                   ENDSR
021000000801     C*----------------------------------------------------*
021100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
021200000801     C*----------------------------------------------------*
021300000801     C     DEFCAM        BEGSR
021400000801     C*
021500150427     C                   CLEAR                   EDIVAB00
021600150427     C                   CLEAR                   EDIVAT00
021700020619     C* Imposto i valori di default...
021800110610     C                   Z-ADD     2221015       VABCCM
021900110610     C                   Z-ADD     2221015       VATCCM
022000110603     C                   Z-ADD     222           VABLNP
022100110603     C                   Z-ADD     222           VATLNP
022200110610     C                   Z-ADD     000           VABCTR
022300110610     C                   MOVEL     '1'           VABCBO
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
024900041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
025000041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
025100041210     C                   ENDIF
025200020619     C                   ENDIF
025300000801     C*
025400000801     C                   ENDSR
025500000801     C*----------------------------------------------------*
025600150427     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
025700000801     C*----------------------------------------------------*
025800040910     C     IMPVABVAT     BEGSR
025900041210     C*
026000041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
026100041210     C                   Z-ADD     *zeros        errore            1 0
026200040910     C*
026300041210     C********
026400050805     C* Tipo record 'KSTA_' (dati destinatario, importi e particolarità consegna)
026500041210     C********
026600050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KSTA_'
026700150427     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
026800041210     C                   IF        FlgNewBol = '1'
026900110714     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
027000110714     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
027100150427     C*
027200150427     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
027300150427     C                   MOVE(P)   vlrMSG        wNomeFile
027400150427     C                   EVAL      VABCMR=%char(datcor)+'-'+%char(oracor) +
027500150427     C                                    ' CMR:' +
027600150427     C                                    %subst(wNomeFile:6:3)
027700150427     C                   EVAL      VABDCM = datcor
027800150427     C                   EVAL      VABDTS = datcor
027900150427     C                   EVAL      VABHMS = oracor
028000150427     C                   EVAL      VABCNT = 1
028100150427     C*
028200150427     C                   WRITE     EDIVAB00
028300041210     C                   EVAL      FlgNewBol = '0'
028400041210     C                   ENDIF
028500041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
028600040910     C                   EXSR      INZVAR
028700040910     C                   EXSR      DEFCAM
028800041210     C                   EVAL      FlgNewBol = '1'
028900040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
029000040928     C                   MOVEL     datcor        VABAAS
029100040928     C                   MOVEL     datcor        VATAAS
029200040928     C                   MOVE      datcor        VABMGS
029300040928     C                   MOVE(P)   vlrpoi        VABFGS
029400040928     C                   MOVE(P)   vlrpoi        VATFGS
029500041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
029600041210     C                   CLEAR                   TRUL33DS
029700041210     C                   EVAL      I33OPE = *zeros
029800041210     C                   EVAL      I33CNU = 302
029900041210     C                   EVAL      I33NUM = 1
030000041210     C                   MOVEL     TRUL33DS      KPJBU
030100041210     C                   CALL      'TRUL33R'
030200041210     C                   PARM                    KPJBA
030300041210     C                   MOVEL     KPJBU         TRUL33DS
030400041210     C                   IF        O33ERR = *zeros
030500041210     C                   Z-ADD     O33NRF        VABNSP
030600041210     C                   Z-ADD     O33NRF        VATNSP
030700041210     C                   ELSE
030800041210     C                   ADD       1             errore
030900041210     C                   EVAL      vinmsg = %trimr(vinmsg)
031000041025     C                             + ' ' + 'VABNSP VATNSP'
031100041210     C                   ENDIF
031200050805     C* ......VABRMA
031300110629     C                   EVAL      VABRMA=%trim(%subst(vindta:52:9))
031400110629     C* ......VABRMN
031500110629     C                   EVAL      PiStr=%trim(%subst(vindta:44:8))
031600110629     C                   EXSR      CHKNUM
031700110629     C                   IF        PiInt=*on
031800110629     C                   Z-ADD     PiVal         VABRMN
031900110629     C                   ELSE
032000110629     C                   Z-ADD     1             VABRMN
032100110629     C                   ADD       1             errore
032200110629     C                   EVAL      vinmsg = %trimr(vinmsg)
032300110629     C                             + ' ' + 'VABRMN'
032400110629     C                   ENDIF
032500050805     C* ......VABRSD/VABRD2
032600050805     C                   MOVEL     *blanks       wRAGSOCDEST      70
032700050805     C                   EVAL      wRAGSOCDEST=%trim(%subst(vindta:61:30))+' '+
032800050805     C                                         %trim(%subst(vindta:91:30))+' '+
032900050805     C                                         %trim(%subst(vindta:121:30))
033000050805     C                   EVAL      VABRSD=%trim(%subst(wRAGSOCDEST:1:35))
033100050805     C                   EVAL      VABRD2=%trim(%subst(wRAGSOCDEST:36:35))
033200050805     C* ......VABIND
033300110610     C                   EVAL      VABIND=%trim(%subst(vindta:151:30))
033400050805     C* ......VABNZD
033500110610     C                   EVAL      VABNZD=%trim(%subst(vindta:181:3))
033600110610     C                   Z-ADD     1             jNAZ
033700110610     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
033800110610     C                   IF        %found
033900110610     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
034000110610     C                   ENDIF
034100050805     C* ......VABLOD
034200110610     C                   EVAL      VABLOD=%trim(%subst(vindta:190:26))
034300110623     C* gestione particolare x provioncia inclusa nella località
034400110623     C                   IF        %subst(%trim(VABLOD):
034500110623     C                             %len(%trim(VABLOD))-2:1) = *blank
034600110623     C                   EVAL      VABPRD = %subst(%trim(VABLOD):
034700110623     C                                      %len(%trim(VABLOD))-1:2)
034800110623     C                   EVAL      VABLOD = %subst(%trim(VABLOD):1:
034900110623     C                                      %len(%trim(VABLOD))-2)
035000110623     C                   ENDIF
035100050805     C* ......VABCAD
035200050805     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
035300050805     C                   EXSR      CHKNUM
035400050805     C                   IF        PiInt=*on
035500050805     C                   Z-ADD     PiVal         Num5_0
035600050805     C                   MOVEL(P)  Num5_0        VABCAD
035700050805     C                   ELSE
035800050805     C                   ADD       1             errore
035900050805     C                   EVAL      vinmsg = %trimr(vinmsg)
036000050805     C                             + ' ' + 'VABCAD'
036100050805     C                   ENDIF
036200110603     C* ......VABCBO
036300110603     C                   IF        %trim(%subst(vindta:330:3)) = '011'
036400110603     C                   EVAL      VABCBO = '2'
036500110603     C                   ENDIF
036600050805     C* ......VABIAS
036700110808     C***                IF        %subst(vindta:334:9) <> *blanks AND
036800110808     C***                          %subst(vindta:334:9) <> *all'0'
036900110808     C***                EVAL      PiStr=%trim(%subst(vindta:334:9))
037000110808     C***                EXSR      CHKNUM
037100110808     C***                IF        PiNum=*on
037200110808     C***                EVAL      PiVal = PiVal / 100                          * gest 2 dec.
037300110808     C***                Z-ADD     PiVal         VABIAS
037400110808     C***                ELSE
037500110808     C***                ADD       1             errore
037600110808     C***                EVAL      vinmsg = %trimr(vinmsg)
037700110808     C***                          + ' ' + 'VABIAS'
037800110808     C***                ENDIF
037900110808     C***                IF        VABIAS > *zeros
038000110808     C***                EVAL      VABVAS = 'EUR'
038100110808     C***                IF        %trim(%subst(vindta:343:3)) = '001'
038200110808     C***                EVAL      VABVAS = 'CHF'
038300110808     C***                ENDIF
038400110808     C***                IF        %trim(%subst(vindta:343:3)) = '002'
038500110808     C***                EVAL      VABVAS = 'USD'
038600110808     C***                ENDIF
038700110808     C***                IF        %trim(%subst(vindta:343:3)) = '003'
038800110808     C***                EVAL      VABVAS = 'GBP'
038900110808     C***                ENDIF
039000110808     C***                IF        %trim(%subst(vindta:343:3)) = '006'
039100110808     C***                EVAL      VABVAS = 'NOK'
039200110808     C***                ENDIF
039300110808     C***                IF        %trim(%subst(vindta:343:3)) = '100'
039400110808     C***                EVAL      VABVAS = 'EUR'
039500110808     C***                ENDIF
039600110808     C***                IF        %trim(%subst(vindta:343:3)) = '114'
039700110808     C***                EVAL      VABVAS = '***'                               Fiorino Ungherese
039800110808     C***                ENDIF
039900110808     C***                ENDIF
040000110808     C***                ENDIF
040100050805     C* ......VABCAS
040200110603     C                   IF        %subst(vindta:368:9) <> *blanks AND
040300110603     C                             %subst(vindta:368:9) <> *all'0'
040400050805     C                   EVAL      PiStr=%trim(%subst(vindta:368:9))
040500050805     C                   EXSR      CHKNUM
040600050805     C                   IF        PiNum=*on
040700110603     C                   EVAL      PiVal = PiVal / 100                          * gest 2 dec.
040800050805     C                   Z-ADD     PiVal         VABCAS
040900050805     C                   MOVEL     '1'           FlgCAS
041000050805     C                   ELSE
041100050805     C                   ADD       1             errore
041200050805     C                   EVAL      vinmsg = %trimr(vinmsg)
041300050805     C                             + ' ' + 'VABCAS'
041400050805     C                   ENDIF
041500050805     C                   IF        VABCAS > *zeros
041600110603     C                   EVAL      VABVCA = 'EUR'
041700110603     C                   IF        %trim(%subst(vindta:377:3)) = '001'
041800050805     C                   EVAL      VABVCA = 'CHF'
041900050805     C                   ENDIF
042000110603     C                   IF        %trim(%subst(vindta:377:3)) = '002'
042100050805     C                   EVAL      VABVCA = 'USD'
042200050805     C                   ENDIF
042300110603     C                   IF        %trim(%subst(vindta:377:3)) = '003'
042400050805     C                   EVAL      VABVCA = 'GBP'
042500050805     C                   ENDIF
042600110603     C                   IF        %trim(%subst(vindta:377:3)) = '006'
042700050805     C                   EVAL      VABVCA = 'NOK'
042800050805     C                   ENDIF
042900110603     C                   IF        %trim(%subst(vindta:377:3)) = '100'
043000050805     C                   EVAL      VABVCA = 'EUR'
043100050805     C                   ENDIF
043200110603     C                   IF        %trim(%subst(vindta:377:3)) = '114'
043300050805     C                   EVAL      VABVCA = '***'                               Fiorino Ungherese
043400050805     C                   ENDIF
043500050805     C                   ENDIF
043600110603     C                   ENDIF
043700050805     C* ......VABDCR
043800110718     C***                IF        %trim(%subst(vindta:328:1)) = 'Q'
043900050805     C                   MOVEL     *blanks       wDCR              8
044000110603     C                   EVAL      wDCR=%subst(vindta:400:2)+
044100110603     C                                  %subst(vindta:398:2)+
044200050805     C                                  %subst(vindta:396:2)+
044300050805     C                                  %subst(vindta:394:2)
044400050805     C                   EVAL      PiStr=wDCR
044500050805     C                   EXSR      CHKNUM
044600050805     C                   IF        PiInt=*on
044700050805     C                   MOVEL     wDCR          VABDCR
044800050805     C                   ELSE
044900050805     C                   ADD       1             errore
045000050805     C                   EVAL      vinmsg = %trimr(vinmsg)
045100050805     C                             + ' ' + 'VABDCR'
045200050805     C                   ENDIF
045300110718     C***                ENDIF
045400050805     C*
045500050805     C                   ENDIF
045600050805     C*
045700050805     C********
045800050805     C* Tipo record 'KZEI_' (colli, peso e natura merce)
045900050805     C********
046000050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KZEI_'
046100050805     C* ......VABNCL
046200050805     C                   EVAL      PiStr=%trim(%subst(vindta:37:5))
046300050805     C                   EXSR      CHKNUM
046400050805     C                   IF        PiInt=*on
046500050805     C                   Z-ADD     PiVal         VABNCL
046600050805     C                   ELSE
046700050805     C                   ADD       1             errore
046800050805     C                   EVAL      vinmsg = %trimr(vinmsg)
046900050805     C                             + ' ' + 'VABNCL'
047000050805     C                   ENDIF
047100050805     C* ......VABNAS
047200050805     C                   EVAL      VABNAS=%trim(%subst(vindta:45:20))
047300050805     C* ......VABPKB
047400050805     C                   EVAL      PiStr=%trim(%subst(vindta:65:5))
047500050805     C                   EXSR      CHKNUM
047600050805     C                   IF        PiNum=*on
047700050805     C                   Z-ADD     PiVal         VABPKB
047800050805     C                   ELSE
047900050805     C                   ADD       1             errore
048000050805     C                   EVAL      vinmsg = %trimr(vinmsg)
048100050805     C                             + ' ' + 'VABPKB'
048200050805     C                   ENDIF
048300110603     C* ......VABVLB
048400110603     C                   IF        %subst(vindta:75:7) <> *blanks AND
048500110603     C                             %subst(vindta:75:7) <> *all'0'
048600110603     C                   EVAL      PiStr=%trim(%subst(vindta:75:7))
048700110603     C                   EXSR      CHKNUM
048800110603     C                   IF        PiNum=*on
048900110603     C                   EVAL      PiVal = PiVal / 1000                         * gest 3 dec
049000110603     C                   Z-ADD     PiVal         VABVLB
049100110603     C                   ELSE
049200110603     C                   ADD       1             errore
049300110603     C                   EVAL      vinmsg = %trimr(vinmsg)
049400110603     C                             + ' ' + 'VABVLB'
049500110603     C                   ENDIF
049600110603     C                   ENDIF
049700050805     C*
049800050805     C                   ENDIF
049900050805     C*
050000050805     C********
050100050805     C* Tipo record 'KPPA_' (barcode)
050200050805     C********
050300050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KPPA_'
050400050805     C* ......VATNOT/VATTRC
050500050805     C                   EVAL      VATTRC='E'
050600050805     C                   EVAL      VATNOT=%trim(%subst(vindta:39:35))
050700150427     C*
050800150427     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
050900150427     C                   MOVE(P)   vlrMSG        wNomeFile
051000150427     C                   EVAL      VATCMR=%char(datcor)+'-'+%char(oracor) +
051100150427     C                                    ' CMR:' +
051200150427     C                                    %subst(wNomeFile:6:3)
051300150427     C                   EVAL      VATCNT = 1
051400150427     C*
051500150427     C                   WRITE     EDIVAT00
051600050805     C*
051700050805     C                   ENDIF
051800050805     C*
051900050805     C********
052000050805     C* Tipo record 'KUTX_' (riferimento mittente e note)
052100050805     C********
052200050805     C                   IF        %trim(%subst(vindta:1:5)) = 'KUTX_'
052300110714     C* ......VABNOT/VABNT2
052400110714     C                   IF        %trim(%subst(vindta:37:2)) = 'AS'
052500110714     C                   EVAL      wNOTE = %trim(wNOTE) +
052600110714     C                                     %trim(%subst(vindta:39:60))
052700110714     C                   ENDIF
052800041210     C*
052900041210     C                   ENDIF
053000040910     C*
053100040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
053200040910     C                   IF        FlgCAS <> '0'
053300040929     C                   IF        VABCBO = '1'
053400040910     C                   EVAL      VABCBO = '4'
053500040910     C                   ELSE
053600040929     C                   EVAL      VABCBO = '6'
053700040910     C                   ENDIF
053800040929     C                   ENDIF
053900040910     C*
054000040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054100040910     C                   EXSR      CHKIMPDIV
054200010202     C*
054300000801     C* Ebbene...
054400000801     C                   ADD       1             §CTRMO
054500010201     C                   IF        errore <> *zeros
054600000801     C                   ADD       1             §CTRNO
054700000801     C                   EVAL      vinflg = '2'
054800000801     C                   ELSE
054900010201     C                   ADD       1             §CTROKVB
055000000801     C                   ENDIF
055100000801     C*
055200000801     C                   ENDSR
055300010202     C*----------------------------------------------------*
055400150427     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
055500010202     C*----------------------------------------------------*
055600020305     C     PREVAT        BEGSR
055700010202     C*
055800150427     C* Compongo il nome del membro da dare al EDIVATWR
055900010202     C                   eval      parmbr = vlrhdl
056000010202     C                   movel     'M'           parmbr
056100041210     C                   eval      parccm = vlrksc
056200010202     C                   eval      paropz = '1'
056300010202     C* Effettuo la chiamata al CLLE preposto
056400150427     C                   call(e)   'TITVEVTC'
056500010202     C                   parm                    parccm
056600010202     C                   parm                    parmbr
056700010202     C                   parm                    paropz
056800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
056900010202     C                   if        %error
057000010202     C                   movel     '1'           chkcall
057100010202     C                   else
057200010202     C                   movel     '0'           chkcall
057300010202     C                   endif
057400010202     C*
057500010202     C                   ENDSR
057600000801     C*----------------------------------------------------*
057700000801     C*  CONTROLLO NUMERICITA' CAMPI
057800000801     C*----------------------------------------------------*
057900000801     C     CHKNUM        BEGSR
058000000801     C*
058100000801     C                   call(e)   'ISNUMERIC'
058200000801     C                   PARM                    PiStr            30
058300041210     C                   PARM      '.'           PiDecChr          1
058400000801     C                   PARM      *ZEROS        PiVal            30 9
058500000801     C                   PARM      '0'           PiInt             1
058600000801     C                   PARM      '0'           PiNum             1
058700000801     C                   IF        %error
058800000801     C                   EVAL      PiInt=*off
058900000801     C                   ENDIF
059000000801     C*
059100000801     C                   ENDSR
059200000801     C***
059300000801
059400011113
059500011113     C*----------------------------------------------------*
059600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
059700011113     C*----------------------------------------------------*
059800011113     C     CHKIMPDIV     BEGSR
059900011113     C*
060000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
060100011113     C                   Z-ADD     *zeros        wrkDec            9 9
060200011113     C*
060300011113     C* Come prima cosa effettuo considerazioni sulla divisa
060400011113     C                   IF        vabIAS > *zeros
060500011113     C                   IF        vabVAS <> 'EUR'
060600011113     C                   EVAL      vabVAS =  'ITL'
060700011113     C                   ENDIF
060800011113     C                   ENDIF
060900011113     C*
061000011113     C                   IF        vabCAS > *zeros
061100011113     C                   IF        vabVCA <> 'EUR'
061200011113     C                   EVAL      vabVCA =  'ITL'
061300011113     C                   ENDIF
061400011113     C                   ENDIF
061500011113     C*
061600011113     C                   IF        vabVMD > *zeros
061700020305     C                   IF        vabVAD <> 'EUR'
061800011113     C                   EVAL      vabVAD =  'ITL'
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
062300011113     C                   Z-ADD     vabIAS        wrkDec
062400011113     C                   IF        wrkDec > *zeros
062500011113     C                   IF        vabVAS = 'ITL'
062600011113     C                   EVAL      vabIAS = *zeros
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
063100011113     C                   Z-ADD     vabCAS        wrkDec
063200011113     C                   IF        wrkDec > *zeros
063300011113     C                   IF        vabVCA = 'ITL'
063400011113     C                   EVAL      vabCAS = *zeros
063500011113     C                   ENDIF
063600011113     C                   ENDIF
063700011113     C*
063800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063900011113     C                   Z-ADD     vabVMD        wrkDec
064000011113     C                   IF        wrkDec > *zeros
064100011113     C                   IF        vabVAD = 'ITL'
064200011113     C                   EVAL      vabVMD = *zeros
064300011113     C                   ENDIF
064400011113     C                   ENDIF
064500011113     C*
064600011113     C                   ENDSR
064700011113     C***
064800011113
064900011113
065000000801
065100000801
065200990920      /TITLE Invio dei dati al punto operativo.
065300010202     C     invio         BEGSR
065400990920     C*
065500150427     C* 1° invio EDIVAT
065600010201     C                   reset                   dscmz
065700010201     C                   move      vlrpoi        cmzdst
065800150427     C                   eval      cmzfld = 'EDIVATWR'
065900010201     C                   eval      cmzmbd = vlrhdl
066000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066100021009     C***                if        prmfir = *blanks
066200150427     C                   eval      cmzfla = 'EDIVAT0F'
066300150427     C                   eval      cmzmba = 'EDIVAT0F'
066400021009     C***                else
066500021009     C***                eval      cmzfla = prmfir
066600021009     C***                eval      cmzmba = prmfir
066700021009     C***                endif
066800010201     C                   eval      cmznrr = *zeros
066900020305     C                   move      §ctrokvt      cmznrr
067000021018     C                   eval      cmzlba = vlrfl1
067100010201     C                   call(e)   'TIS711C'
067200010201     C                   parm                    dscmz
067300010201     C                   parm      *blanks       esito
067400010205     C                   if        %error
067500010205     C                             or cmzerr = '1'
067600010205     C                             or esito  = '1'
067700010205     C                   eval      wrkesito = '3'
067800010205     C                   else
067900010201     C*
068000150427     C* 2° invio EDICVAB
068100010201     C                   reset                   dscmz
068200010201     C                   move      vlrpoi        cmzdst
068300010201     C                   eval      cmzfld = vlrfou
068400010201     C                   eval      cmzmbd = vlrhdl
068500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068600021009     C***                if        prmfir = *blanks
068700150427     C                   eval      cmzfla = 'EDIVAB0F'
068800150427     C                   eval      cmzmba = 'EDIVAB0F'
068900021009     C***                else
069000021009     C***                eval      cmzfla = prmfir
069100021009     C***                eval      cmzmba = prmfir
069200021009     C***                endif
069300010201     C                   eval      cmznrr = *zeros
069400010201     C                   move      §ctrokvb      cmznrr
069500021018     C                   eval      cmzlba = vlrfl1
069600010201     C                   call(e)   'TIS711C'
069700010201     C                   parm                    dscmz
069800010201     C                   parm      *blanks       esito
069900010201     C                   if        %error
070000010201     C                             or cmzerr = '1'
070100010201     C                             or esito  = '1'
070200010201     C                   eval      wrkesito = '3'
070300010201     C                   endif
070400010205     C                   endif
070500990920     C*
070600000613     C                   ENDSR
070700000613     C***
070800070411
070900070411     C     *pssr         BEGSR
071000070411     C*
071100070411     C                   if        %open(tivin00r)
071200070411     C                   close     tivin00r
071300070411     C                   endif
071400150427     C                   if        %open(edivabwr)
071500150427     C                   close     edivabwr
071600070411     C                   endif
071700150427     C                   if        %open(edivatwr)
071800150427     C                   close     edivatwr
071900070411     C                   endif
072000070411     C*
072100070411     C* Effettuo la chiamata al CLLE preposto
072200150427     C                   call(e)   'TITVEVTC'
072300070411     C                   parm                    parccm
072400070411     C                   parm                    parmbr
072500070411     C                   parm      '2'           paropz
072600070411     C*
072700070411     C                   eval      wrkesito = '2'
072800070411     C*
072900070411     C                   seton                                        LR
073000070411     C*
073100070411     C                   ENDSR     '*CANCL'
073200070411     C***
073300070411
073400990910
073500000613     C     *inzsr        BEGSR
073600990910     C*
073700990910     C     *entry        plist
073800990920     C                   parm                    tivlrds
073900990921     C                   parm      wrkesito      esito
074000000724     C                   parm                    prmlit
074100000710     C                   parm                    prmfir
074200000613     C*
074300000830     C* CALCOLA LA DATA CORRENTE
074400000830     C                   time                    wn14             14 0
074500150427     C                   movel     wn14          oracor            6 0          *ORA
074600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
074700000830     C                   z-add     wn8           g08dat
074800000830     C                   z-add     *zeros        g08inv
074900000830     C                   movel     '0'           g08err
075000000830     C                   call      'XSRDA8'
075100000830     C                   parm                    wlbda8
075200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075300041210     C*
075400041210     C* Chiave su TABEL00F - parziale
075500041210     C     KEYtabP       KLIST
075600041210     C                   KFLD                    tblKUT
075700041210     C                   KFLD                    tblCOD
075800000830     C*
075900000613     C                   ENDSR
076000000613     C***
