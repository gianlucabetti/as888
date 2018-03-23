000100150429      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVADWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVADwwr  O    E             DISK    usropn
000700150429     FFIVATwwr  O    E             DISK    usropn
000800990908
000900010201     D*------------
001000010201     D* SCHIERE
001100010201     D*------------
001200021010     D skcdpvad        S             15    DIM(2000)
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400990915     D wrkdata         s               d
002500990915     D wrkora          s               t
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700010201     D depspe          s             16    INZ(*blanks)
002800010213     D depdat          s              8  0 INZ(*zeros)
002900010213     D depaas          s              4    INZ(*zeros)
003000010201     D curspe          s             16    INZ(*blanks)
003100010201     D depcdp          s             15    INZ(*blanks)
003200010305     D cntpez          s              5  0 INZ(*zeros)
003300010305     D cntcdp          s              5  0 INZ(*zeros)
003400010305     D depcnt          s              5  0 INZ(*zeros)
003500010201     D i               s              5i 0
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000010202     D s_VABCCM        s                   LIKE(VABCCM)
004100010202     D s_VABLNP        s                   LIKE(VABLNP)
004200010202     D s_VABAAS        s                   LIKE(VABAAS)
004300010202     D s_VABNRS        s                   LIKE(VABNRS)
004400010202     D s_VABNSP        s                   LIKE(VABNSP)
004500021113     D s_VABFGS        s                   LIKE(VABFGS)
004600000801
004700000801     D                 DS                  INZ
004800000801     D  ALFA35UNO              1     35
004900000801     D  ALFA35DUE             36     70
005000000801     D  ALFA70                 1     70
005100000801
005200010213     D                 DS                  INZ
005300010213     D  aaaa                   1      4  0
005400010213     D  mmgg                   5      8  0
005500010213     D  mm                     5      6  0
005600010213     D  gg                     7      8  0
005700010213     D  datawrk                1      8  0
005800010213
005900010213     D  Num8_0         s              8  0
006000000801     D  Num5_0         s              5  0
006100150429     D wVATNOT_IJ      s             70
006200150429     D wVATNOT_S       s                   LIKE(VATNOT)
006300150515     D wVATNOT_E       s                   LIKE(VATNOT)
006400150429     D w70             s             70
006500000830
006600000830     D*------------------
006700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
006800000830     D*------------------
006900000830     D WLBDA8          DS                  INZ
007000000830     D  G08DAT                 1      8  0
007100000830     D  G08INV                 9     16  0
007200000830     D  G08ERR                17     17
007300000830     D  G08TGI                18     22  0
007400000830     D*
007500990908
007600010201
007700010201
007800990915     C                   time                    wrkdata
007900990915     C                   time                    wrkora
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400010201     C                   EXSR      RWLAVA                                       LETT/SCR. VAB
008500000613     C*
008600010202     C* Effettuo la chiamata al CLLE preposto
008700010202     C                   call(e)   'TIS7LAVC'
008800010202     C                   parm                    parccm
008900010202     C                   parm                    parmbr
009000010202     C                   parm      '2'           paropz
009100000616     C*
009200000801     C
009300010201     C                   seton                                        LR
009400000801
009500910830     C*--------------------------------------------------------
009600150429     C* RWLAVA  LEGGE tivin00r E SCRIVE FIVABWWR FIVADWWR FIVATWWR  *
009700910830     C*--------------------------------------------------------
009800010201     C     RWLAVA        BEGSR
009900990910     C*
010000990914     C                   if        not %open(tivin00r)
010100990908     C                   open      tivin00r
010200990914     C                   endif
010300021113     C                   if        not %open(fivabwwr)
010400021113     C                   open      fivabwwr
010500990914     C                   endif
010600150429     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR e FIVATWWR
010700150429     C                   exsr      prevadvat
010800010201     C*
010900010202     C                   if        chkcall = '0'
011000010202     C*
011100021113     C                   if        not %open(fivadwwr)
011200021113     C                   open      fivadwwr
011300010201     C                   endif
011400150429     C*
011500150429     C                   if        not %open(fivatwwr)
011600150429     C                   open      fivatwwr
011700150429     C                   endif
011800990910     C*
011900010201     C                   clear                   §CTROKVB          5 0
012000010201     C                   clear                   §CTROKVD          5 0
012100150429     C                   clear                   §CTROKVT          5 0
012200000801     C                   clear                   §CTRMO            5 0
012300000801     C                   clear                   §CTRNO            5 0
012400010201     C                   clear                   skcdpvad
012500990910     C*
012600921023     C                   DO        *HIVAL
012700990913     C*
012800990915     C                   READ      tivin00r                               70
012900000905     C                   if        vindta > *blanks
013000000613     C                   add       1             rrnum
013100000801     C*
013200000801     C                   if        *in70 = *off
013300000801     C                             and
013400000801     C                             (vinflg = *blanks
013500000801     C                              or vinflg = '0'
013600000801     C                              or vinflg = '2')
013700000801     C*
013800000801     C                   clear                   vinmsg
013900000801     C                   eval      vinflg = '1'
014000010305     C*
014100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014200011115     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
014300010305     C                   MOVEL(p)  PiStr         curspe
014400010305     C*
014500010305     C                   if        depspe = *blanks                             => 1° giro
014600010305     C                   eval      depspe = curspe                              => memorizz. spediz
014700010305     C                   exsr      impvab
014800010305     C                   exsr      carvad
014900150429     C                   exsr      carvat
015000010305     C                   else
015100010305     C                   if        curspe <> depspe                             => rottura di spediz
015200010305     C                   eval      depspe = curspe                              => memorizz. spediz
015300010305     C                   exsr      scrivi
015400010305     C                   else                                                   => x stessa spediz
015500010305     C                   exsr      carvad                                       => carico VAD
015600150429     C                   exsr      carvat                                       => carico VAD
015700010305     C                   endif
015800010305     C                   endif
015900010305     C                   endif
016000000905     C*
016100000905     C                   else
016200000905     C                   eval      vinflg = '1'
016300000905     C                   endif
016400000905     C*
016500000905     C  N70              update    tivin000
016600000905     C*
016700991022     C  N70              ENDdo
016800010202     C*
016900150429     C* Scarico i VAB i VAD e i VAT rimasti "in sospeso"
017000010305     C                   exsr      scrivi
017100010202     C*
017200010202     C                   endif
017300990910
017400990910     C* Se non ci sono record con errori ...
017500000710     C                   if        §ctrno = 0
017600990910     C* ... restituisco esito OK.
017700990921     C                   eval      wrkesito = '0'
017800990910     C                   else
017900010201     C                   if        §ctrokvb > 0
018000990921     C                   eval      wrkesito = '1'
018100000710     C                   else
018200000710     C                   eval      wrkesito = '2'
018300990910     C                   endif
018400000710     C                   endif
018500990910     C*
018600990914     C                   if        %open(tivin00r)
018700990908     C                   close     tivin00r
018800990914     C                   endif
018900021113     C                   if        %open(fivabwwr)
019000021113     C                   close     fivabwwr
019100990914     C                   endif
019200021113     C                   if        %open(fivadwwr)
019300021113     C                   close     fivadwwr
019400010201     C                   endif
019500150429     C                   if        %open(fivatwwr)
019600150429     C                   close     fivatwwr
019700150429     C                   endif
019800990910     C*
019900010201     C                   if        §ctrokvb > 0
020000000724     C                             and vlrpoi <> *zeros
020100010202     C                   exsr      invio
020200990920     C                   endif
020300990920     C*
020400910830     C                   ENDSR
020500000613     C***
020600010305
020700010305     C*----------------------------------------------------*
020800010305     C*  SCARICAMENTO BUFFER RECORDS VAB/VAD
020900010305     C*----------------------------------------------------*
021000010305     C     SCRIVI        BEGSR
021100010305     C*
021200010305     C                   z-add     VABCCM        s_VABCCM
021300010305     C                   z-add     VABLNP        s_VABLNP
021400010305     C                   z-add     VABAAS        s_VABAAS
021500021113     C                   z-add     VABFGS        s_VABFGS
021600010305     C                   z-add     VABNRS        s_VABNRS
021700010305     C                   z-add     VABNSP        s_VABNSP
021800010305     C                   exsr      wrivad                                       => scarico i VAD
021900150429     C                   exsr      wrivat                                       => scarico i VAT
022000021113     C                   write     fivab000                                     => scarico il VAB
022100010305     C                   z-add     *zeros        cntpez
022200010305     C                   exsr      impvab                                       => ri-valorizzo VAB
022300010305     C                   exsr      carvad                                       => carico VAD
022400150429     C                   exsr      carvat                                       => carico VAD
022500010305     C*
022600010305     C                   ENDSR
022700990920
022800000801     C*----------------------------------------------------*
022900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023000000801     C*----------------------------------------------------*
023100010201     C     INZVAR        BEGSR
023200000801     C*
023300000801     C                   MOVEL     *blanks       ALFA70
023400010201     C                   Z-ADD     *zeros        Num5_0
023500150429     C                   MOVEL     *blanks       wVATNOT_IJ
023600150429     C                   MOVEL     *blanks       wVATNOT_S
023700150515     C                   MOVEL     *blanks       wVATNOT_E
023800000801     C*
023900000801     C                   ENDSR
024000020326     C*----------------------------------------------------*
024100020326 xxx C*  IMPOSTAZIONE CAMPI COSTANTI
024200020326     C*----------------------------------------------------*
024300020326     C     DEFCAM        BEGSR
024400020326     C*
024500020326     C* Inizializzo il buffer del record da scrivere
024600021113     C                   CLEAR                   FIVAB000
024700020326     C* Imposto i valori di default...
024800020326     C                   EVAL      VABCCM = 0491999
024900020326     C                   EVAL      VADCCM = 0491999
025000150429     C                   EVAL      VATCCM = 0491999
025100020326     C                   EVAL      VABLNP = 049
025200020326     C                   EVAL      VADLNP = 049
025300150429     C                   EVAL      VATLNP = 049
025400020326     C                   EVAL      VABCTR = 000
025500020326     C* ... e poi verifico se sono stati passati come parametri
025600020326     C                   IF        vlrppt > *blanks
025700020326     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025800020326     C                   EXSR      CHKNUM
025900020326     C                   IF        PiInt=*on
026000020326     C                   Z-ADD     PiVal         VABCCM
026100020326     C                   Z-ADD     PiVal         VADCCM
026200150429     C                   Z-ADD     PiVal         VATCCM
026300020326     C                   ENDIF
026400020326     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026500020326     C                   EXSR      CHKNUM
026600020326     C                   IF        PiInt=*on
026700020326     C                   Z-ADD     PiVal         VABLNP
026800020326     C                   Z-ADD     PiVal         VADLNP
026900150429     C                   Z-ADD     PiVal         VATLNP
027000020326     C                   ENDIF
027100020326     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027200020326     C                   EXSR      CHKNUM
027300020326     C                   IF        PiInt=*on
027400020326     C                   Z-ADD     PiVal         VABCTR
027500020326     C                   ENDIF
027600020326     C                   ENDIF
027700020326     C*
027800020326     C                   ENDSR
027900000801     C*----------------------------------------------------*
028000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028100000801     C*----------------------------------------------------*
028200010201     C     IMPVAB        BEGSR
028300000801     C*
028400010305     C                   EXSR      INZVAR
028500010305     C                   EXSR      DEFCAM
028600010305     C*
028700000801     C                   Z-ADD     *zeros        errore            1 0
028800000830     C                   MOVEL     datcor        VABAAS
028900010201     C                   MOVEL     datcor        VADAAS
029000150429     C                   MOVEL     datcor        VATAAS
029100021113     C                   MOVE(P)   vlrpoi        VABFGS
029200021113     C                   MOVE(P)   vlrpoi        VADFGS
029300150429     C                   MOVE(P)   vlrpoi        VATFGS
029400010213     C                   MOVEL     VABAAS        depaas
029500000830     C                   MOVE      datcor        VABMGS
029600010201     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
029700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029900020117     C* ==
030000010201     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
030100010202     C                   EVAL      VABRMO=%trim(%subst(vindta:92:15))
030200150515     C* CCM
030300150515     C                   EVAL      PiStr=%trim(%subst(vindta:1:11))
030400150515     C                   EXSR      CHKNUM
030500150515     C                   IF        PiInt=*on
030600150515     C                   Z-ADD     PiVal         VABCCM
030700150515     C                   Z-ADD     PiVal         VADCCM
030800150515     C                   Z-ADD     PiVal         VATCCM
030900150604     C                   MOVEL     VABCCM        VABFGS
031000150604     C                   MOVEL     VADCCM        VADFGS
031100150604     C                   MOVEL     VATCCM        VATFGS
031200150604     C                   Z-ADD     VABFGS        VABLNP
031300150604     C                   Z-ADD     VADFGS        VADLNP
031400150604     C                   Z-ADD     VATFGS        VATLNP
031500150515     C                   ELSE
031600150515     C                   Z-ADD     0             VABCCM
031700150604     C                   Z-ADD     0             VABFGS
031800150604     C                   Z-ADD     0             VABLNP
031900150515     C                   ADD       1             errore
032000150515     C                   EVAL      vinmsg = %trimr(vinmsg)
032100150515     C                             + ' ' + 'VABCCM VADCCM VATCCM'
032200150604     C                             + ' VABFGS VADFGS VATFGS'
032300150604     C                             + ' VABLNP VADLNP VATLNP'
032400150515     C                   ENDIF
032500150515
032600010201     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
032700000801     C                   EXSR      CHKNUM
032800000801     C                   IF        PiInt=*on
032900000801     C                   Z-ADD     PiVal         Num5_0
033000010208     C                   MOVEL(p)  Num5_0        VABCAD
033100000801     C                   ELSE
033200000801     C                   ADD       1             errore
033300000801     C                   EVAL      vinmsg = %trimr(vinmsg)
033400000801     C                             + ' ' + 'VABCAD'
033500000801     C                   ENDIF
033600010201     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
033700010201     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
033800010201     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
033900010201     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
034000010201     C                   EXSR      CHKNUM
034100010201     C                   IF        PiInt=*on
034200010201     C                   Z-ADD     PiVal         Num5_0
034300010208     C                   MOVE      Num5_0        VABNCL
034400010201     C                   ELSE
034500010201     C                   ADD       1             errore
034600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034700010201     C                             + ' ' + 'VABNCL'
034800010201     C                   ENDIF
034900150515     C*
035000150515     C                   EVAL      VABCTM = 'DT'
035100150515     C* se i colli sono 1 e il codice cliente è tra questa lista:
035200150518     C* 0183828, 1152975, 0962194, 1663026
035300150515     C* la gestione segnacollo è diskC
035400150515     C                   IF        VABNCL = 1 and
035500150515     C                             (VABCCM = 0183828 or VABCCM = 1152975 or
035600150518     C                              VABCCM = 0962194 or VABCCM = 1663026)
035700150515     C                   EVAL      VABCTM = '7Q'
035800150515     C                   EVAL      wVATNOT_E = %trim(%subst(vindta:187:10))
035900150515     C                   ENDIF
036000150515     C*
036100010201     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
036200010201     C                   EXSR      CHKNUM
036300010201     C                   IF        PiNum=*on
036400011115     C                   Z-ADD     PiVal         VABPKB
036500010201     C                   ELSE
036600010201     C                   ADD       1             errore
036700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036800010201     C                             + ' ' + 'VABPKB'
036900010201     C                   ENDIF
037000150629     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
037100010201     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
037200010201     C                   EXSR      CHKNUM
037300010201     C                   IF        PiInt=*on
037400011115     C                   Z-ADD     PiVal         VABRMN
037500010201     C                   ELSE
037600010201     C                   ADD       1             errore
037700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037800010201     C                             + ' ' + 'VABRMN'
037900010201     C                   ENDIF
038000010201     C                   EVAL      ALFA70=%trim(%subst(vindta:197:80))
038100010201     C                   MOVEL     ALFA35UNO     VABNOT
038200010201     C                   MOVEL     ALFA35DUE     VABNT2
038300011115     C*
038400011115     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
038500011115     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
038600010201     C                   EXSR      CHKNUM
038700010201     C                   IF        PiNum=*on
038800011115     C                   Z-ADD     PiVal         VABCAS
038900030509     C                   IF        VABCAS > *zeros
039000030506     C                   EVAL      VABTIC = 'BM'
039100030509     C                   ENDIF
039200010201     C                   ELSE
039300010201     C                   ADD       1             errore
039400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039500010201     C                             + ' ' + 'VABCAS'
039600010201     C                   ENDIF
039700011115     C
039800011115     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
039900011115     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
040000010201     C                   EXSR      CHKNUM
040100010201     C                   IF        PiNum=*on
040200011115     C                   Z-ADD     PiVal         VABIAS
040300010201     C                   ELSE
040400010201     C                   ADD       1             errore
040500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040600010201     C                             + ' ' + 'VABIAS'
040700010201     C                   ENDIF
040800011115     C*
040900011115     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
041000010202     C                   IF        PiStr>*blanks
041100010201     C                   EXSR      CHKNUM
041200010201     C                   IF        PiNum=*on
041300011115     C                   Z-ADD     PiVal         VABVLB
041400010201     C                   ELSE
041500010201     C                   ADD       1             errore
041600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
041700010201     C                             + ' ' + 'VABVLB'
041800010201     C                   ENDIF
041900010202     C                   ENDIF
042000011115     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
042100010201     C                   EXSR      CHKNUM
042200010201     C                   IF        PiInt=*on
042300011115     C                   Z-ADD     PiVal         VABNSP
042400011115     C                   Z-ADD     PiVal         VADNSP
042500150429     C                   Z-ADD     PiVal         VATNSP
042600010201     C                   ELSE
042700010201     C                   ADD       1             errore
042800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042900010201     C                             + ' ' + 'VABNSP'
043000010201     C                   ENDIF
043100010213     C* AGGIUNTA: gestione data consegna richiesta
043200010213     C                   EVAL      PiStr=depaas + %trim(%subst(vindta:203:2))+
043300010213     C                                            %trim(%subst(vindta:200:2))
043400010213     C                   EXSR      CHKNUM
043500010213     C                   IF        PiInt=*on
043600010213     C                   Z-ADD     PiVal         Num8_0
043700010213     C                   MOVEL(p)  Num8_0        depdat
043800010219     C                   z-add     depdat        datawrk
043900010213     C                   IF        depdat < datcor
044000010214     C                   add       1             aaaa
044100010219     C                   ENDIF
044200010214     C* A questo punto eseguo controllo di validità della data così ottenuta
044300010214     C                   z-add     *zeros        G08DAT
044400010214     C                   z-add     datawrk       G08INV
044500010214     C                   movel     '3'           G08ERR
044600010214     C                   call      'XSRDA8'
044700010214     C                   parm                    WLBDA8
044800010214     C     G08ERR        ifeq      '1'
044900010214     C                   else
045000010213     C                   z-add     datawrk       VABDCR
045100091126     C                   eval      VABTCR = 'P'
045200010214     C                   endif
045300010214     C                   ENDIF
045400010205     C*
045500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045600010205     C                   IF        VABCAS > *zeros
045700010205     C                   EVAL      VABCBO = '4'
045800010205     C                   ELSE
045900010205     C                   EVAL      VABCBO = '1'
046000010205     C                   ENDIF
046100010205     C*
046200150515     C***                EVAL      VABCTM = 'DT'
046300011113     C*
046400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046500011113     C                   EXSR      CHKIMPDIV
046600010202     C*
046700000801     C* Ebbene...
046800000801     C                   ADD       1             §CTRMO
046900010201     C                   IF        errore <> *zeros
047000000801     C                   ADD       1             §CTRNO
047100000801     C                   EVAL      vinflg = '2'
047200000801     C                   ELSE
047300010201     C                   ADD       1             §CTROKVB
047400000801     C                   ENDIF
047500000801     C*
047600000801     C                   ENDSR
047700010201     C*----------------------------------------------------*
047800021113     C*  CARICAMENTO CAMPI DA FLAT FILE (X FIVAD)
047900010201     C*----------------------------------------------------*
048000010201     C     CARVAD        BEGSR
048100010201     C*
048200010201     C* Carico la schiera dei codici prodotto per cliente/spedizione
048300010201     C                   add       1             i
048400041130     C                   eval      skcdpvad(i)=%subst(vindta:331:8)
048500010201     C*
048600010201     C                   ENDSR
048700150429     C*----------------------------------------------------*
048800150429     C*  CARICAMENTO CAMPI DA FLAT FILE (X FIVAT)
048900150429     C*----------------------------------------------------*
049000150429     C     CARVAT        BEGSR
049100150429     C*
049200150429     C                   if        %trim(%subst(vindta:651:16)) <> *blanks
049300150429     C                   eval      wVATNOT_S = %trim(%subst(vindta:651:16))
049400150429     C                   endif
049500150429     C*
049600150429     C                   if        %trim(%subst(vindta:667:50)) <> *blanks
049700150429     C                   eval      wVATNOT_IJ = %trim(%subst(vindta:667:50))
049800150429     C                   endif
049900150515     C*
050000150515     C* wVATNOT_E non lo valorizzo qui ma nella routine IMPVAB
050100150429     C*
050200150429     C                   ENDSR
050300010201     C*----------------------------------------------------*
050400021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAD)
050500010201     C*----------------------------------------------------*
050600010305     C     WRIVAD        BEGSR
050700010201     C*
050800010201     C* Ordino la schera
050900010201     C                   sorta     skcdpvad
051000010201     C*
051100010201     C                   z-add     1             i
051200010201     C                   z-add     *zeros        cntcdp
051300010201     C* Mi posiziono sul primo elemento pieno della schiera e porto avanti l'indice
051400010202     C                   dow       skcdpvad(i) = *blanks and
051500021010     C                             i < 2000
051600010201     C                   add       1             i
051700010201     C                   enddo
051800010202     C*
051900021010     C                   if        i <= 2000
052000010201     C* Mi salvo il primo codice prodotto
052100041130     C                   eval      depcdp=%subst(skcdpvad(i):1:8)
052200010305     C* Inizio il ciclo principale
052300021010     C                   dow       i <= 2000
052400010305     C                   z-add     1             depcnt
052500010305     C                   eval      cntpez = cntpez + depcnt
052600010201     C* Effettuo considerazioni sulla totalizzazione del dettaglio colli per codice prodotto
052700041130     C                   if        depcdp = %subst(skcdpvad(i):1:8)
052800010305     C                   add       depcnt        cntcdp
052900010201     C                   else
053000021113     C                   clear                   FIVAD000
053100021113     C* Valorizzo l buffer di scrittura del FIVAD
053200010201     C                   eval      VADCCM = s_VABCCM
053300010201     C                   eval      VADLNP = s_VABLNP
053400010201     C                   eval      VADAAS = s_VABAAS
053500021113     C                   eval      VADFGS = s_VABFGS
053600010201     C                   eval      VADNRS = s_VABNRS
053700010201     C                   eval      VADNSP = s_VABNSP
053800010201     C                   eval      VADCDP = depcdp
053900010201     C                   eval      VADNCL = cntcdp
054000021113     C                   write     FIVAD000
054100041130     C                   eval      depcdp=%subst(skcdpvad(i):1:8)
054200010305     C                   z-add     depcnt        cntcdp
054300010201     C                   add       1             §CTROKVD
054400010201     C                   endif
054500010201     C                   add       1             i
054600010201     C                   enddo
054700010305     C*
054800010305     C* A questo punto so qunti sono i colli effettivi della testata della bolla
054900010305     C                   z-add     cntpez        VABNCL
055000010202     C*
055100010305     C* Quando esco dal ciclo scarico gli ultimi codici prodotto rimasti in "sospeso"
055200010202     C                   eval      VADCCM = s_VABCCM
055300010202     C                   eval      VADLNP = s_VABLNP
055400010202     C                   eval      VADAAS = s_VABAAS
055500021113     C                   eval      VADFGS = s_VABFGS
055600010202     C                   eval      VADNRS = s_VABNRS
055700010202     C                   eval      VADNSP = s_VABNSP
055800010202     C                   eval      VADCDP = depcdp
055900010202     C                   eval      VADNCL = cntcdp
056000021113     C                   write     FIVAD000
056100010202     C                   add       1             §CTROKVD
056200010202     C*
056300010202     C                   endif
056400010202     C*
056500010202     C* Re-inizializzo la schiera ed il suo indice
056600010202     C                   z-add     *zeros        i
056700010202     C                   clear                   skcdpvad
056800010201     C*
056900010201     C                   ENDSR
057000150429     C*----------------------------------------------------*
057100150429     C*  SCARICAMENTO BUFFER RECORDS VAT
057200150429     C*----------------------------------------------------*
057300150429     C     WRIVAT        BEGSR
057400150429     C*
057500150429     C* Valorizzo l buffer di scrittura del FIVAT
057600150429     C                   eval      VATCCM = s_VABCCM
057700150429     C                   eval      VATLNP = s_VABLNP
057800150429     C                   eval      VATAAS = s_VABAAS
057900150429     C                   eval      VATFGS = s_VABFGS
058000150429     C                   eval      VATNRS = s_VABNRS
058100150429     C                   eval      VATNSP = s_VABNSP
058200150429     C*
058300150429     C                   if        wVATNOT_IJ <> *blanks
058400150429     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
058500150429     C                   eval      VATTRC = 'I'
058600150429     C                   write     FIVAT000
058700150429     C                   add       1             §CTROKVT
058800150429     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
058900150429     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
059000150429     C                   eval      VATTRC = 'J'
059100150429     C                   write     FIVAT000
059200150429     C                   add       1             §CTROKVT
059300150429     C                   endif
059400150429     C                   endif
059500150429     C*
059600150429     C                   if        wVATNOT_S <> *blanks
059700150429     C                   eval      VATNOT = wVATNOT_S
059800150429     C* i 2 flag devono essere blank
059900150429     C                   EVAL      %subst(VATNOT:17:1)=*blank
060000150429     C                   EVAL      %subst(VATNOT:18:1)=*blank
060100150429     C                   eval      VATTRC = 'S'
060200150429     C                   write     FIVAT000
060300150429     C                   add       1             §CTROKVT
060400150429     C                   endif
060500150429     C*
060600150515     C                   if        wVATNOT_E <> *blanks
060700150515     C                   eval      VATNOT = wVATNOT_E
060800150515     C                   eval      VATTRC = 'E'
060900150515     C                   write     FIVAT000
061000150515     C                   add       1             §CTROKVT
061100150515     C                   endif
061200150515     C*
061300150429     C                   ENDSR
061400010202     C*----------------------------------------------------*
061500150429     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR e FIVATWWR
061600010202     C*----------------------------------------------------*
061700150429     C     PREVADVAT     BEGSR
061800010202     C*
061900150429     C* Compongo il nome del membro da dare al FIVADWWR e FIVATWWR
062000010202     C                   eval      parmbr = vlrhdl
062100010202     C                   movel     'M'           parmbr
062200010203     C                   eval      parccm = '00491999'
062300010202     C                   eval      paropz = '1'
062400010202     C* Effettuo la chiamata al CLLE preposto
062500010202     C                   call(e)   'TIS7LAVC'
062600010202     C                   parm                    parccm
062700010202     C                   parm                    parmbr
062800010202     C                   parm                    paropz
062900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063000010202     C                   if        %error
063100010202     C                   movel     '1'           chkcall
063200010202     C                   else
063300010202     C                   movel     '0'           chkcall
063400010202     C                   endif
063500010202     C*
063600010202     C                   ENDSR
063700000801     C*----------------------------------------------------*
063800000801     C*  CONTROLLO NUMERICITA' CAMPI
063900000801     C*----------------------------------------------------*
064000000801     C     CHKNUM        BEGSR
064100000801     C*
064200000801     C                   call(e)   'ISNUMERIC'
064300000801     C                   PARM                    PiStr            30
064400000801     C                   PARM      ','           PiDecChr          1
064500000801     C                   PARM      *ZEROS        PiVal            30 9
064600000801     C                   PARM      '0'           PiInt             1
064700000801     C                   PARM      '0'           PiNum             1
064800000801     C                   IF        %error
064900000801     C                   EVAL      PiInt=*off
065000000801     C                   ENDIF
065100000801     C*
065200000801     C                   ENDSR
065300000801     C***
065400000801
065500011113
065600011113     C*----------------------------------------------------*
065700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065800011113     C*----------------------------------------------------*
065900011113     C     CHKIMPDIV     BEGSR
066000011113     C*
066100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066200011113     C                   Z-ADD     *zeros        wrkDec            9 9
066300011113     C*
066400011113     C* Come prima cosa effettuo considerazioni sulla divisa
066500011113     C                   IF        vabIAS > *zeros
066600011113     C                   IF        vabVAS <> 'EUR'
066700011113     C                   EVAL      vabVAS =  'ITL'
066800011113     C                   ENDIF
066900011113     C                   ENDIF
067000011113     C*
067100011113     C                   IF        vabCAS > *zeros
067200011113     C                   IF        vabVCA <> 'EUR'
067300011113     C                   EVAL      vabVCA =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C                   IF        vabVMD > *zeros
067800011113     C                   IF        vabVAD <> 'EUR'
067900011113     C                   EVAL      vabVAD =  'ITL'
068000011113     C                   ENDIF
068100011113     C                   ENDIF
068200011113     C*
068300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068400011113     C                   Z-ADD     vabIAS        wrkDec
068500011113     C                   IF        wrkDec > *zeros
068600011113     C                   IF        vabVAS = 'ITL'
068700011113     C                   EVAL      vabIAS = *zeros
068800011113     C                   ENDIF
068900011113     C                   ENDIF
069000011113     C*
069100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069200011113     C                   Z-ADD     vabCAS        wrkDec
069300011113     C                   IF        wrkDec > *zeros
069400011113     C                   IF        vabVCA = 'ITL'
069500011113     C                   EVAL      vabCAS = *zeros
069600011113     C                   ENDIF
069700011113     C                   ENDIF
069800011113     C*
069900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070000011113     C                   Z-ADD     vabVMD        wrkDec
070100011113     C                   IF        wrkDec > *zeros
070200011113     C                   IF        vabVAD = 'ITL'
070300011113     C                   EVAL      vabVMD = *zeros
070400011113     C                   ENDIF
070500011113     C                   ENDIF
070600011113     C*
070700011113     C                   ENDSR
070800011113     C***
070900011113
071000011113
071100000801
071200000801
071300990920      /TITLE Invio dei dati al punto operativo.
071400010202     C     invio         BEGSR
071500150429     C*
071600150429     C* 1° invio FIVAT
071700150429     C                   reset                   dscmz
071800150429     C                   move      vlrpoi        cmzdst
071900150429     C                   eval      cmzfld = 'FIVATWWR'
072000150429     C                   eval      cmzmbd = vlrhdl
072100150429     C                   eval      %subst(cmzmbd:1:1) = 'M'
072200150429     C***                if        prmfir = *blanks
072300150429     C                   eval      cmzfla = 'FIVAT00F'
072400150429     C                   eval      cmzmba = 'FIVAT00F'
072500150429     C***                else
072600150429     C***                eval      cmzfla = prmfir
072700150429     C***                eval      cmzmba = prmfir
072800150429     C***                endif
072900150429     C                   eval      cmznrr = *zeros
073000150429     C                   move      §ctrokvt      cmznrr
073100150429     C                   eval      cmzlba = vlrfl1
073200150429     C                   call(e)   'TIS711C'
073300150429     C                   parm                    dscmz
073400150429     C                   parm      *blanks       esito
073500150429     C                   if        %error
073600150429     C                             or cmzerr = '1'
073700150429     C                             or esito  = '1'
073800150429     C                   eval      wrkesito = '3'
073900150429     C                   else
074000990920     C*
074100150429     C* 2° invio FIVAD
074200010201     C                   reset                   dscmz
074300010201     C                   move      vlrpoi        cmzdst
074400021113     C                   eval      cmzfld = 'FIVADWWR'
074500010201     C                   eval      cmzmbd = vlrhdl
074600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074700021009     C***                if        prmfir = *blanks
074800021113     C                   eval      cmzfla = 'FIVAD00F'
074900021113     C                   eval      cmzmba = 'FIVAD00F'
075000021009     C***                else
075100021009     C***                eval      cmzfla = prmfir
075200021009     C***                eval      cmzmba = prmfir
075300021009     C***                endif
075400010201     C                   eval      cmznrr = *zeros
075500010201     C                   move      §ctrokvd      cmznrr
075600021111     C                   eval      cmzlba = vlrfl1
075700010201     C                   call(e)   'TIS711C'
075800010201     C                   parm                    dscmz
075900010201     C                   parm      *blanks       esito
076000010205     C                   if        %error
076100010205     C                             or cmzerr = '1'
076200010205     C                             or esito  = '1'
076300010205     C                   eval      wrkesito = '3'
076400010205     C                   else
076500010201     C*
076600150429     C* 3° invio FIVAB
076700010201     C                   reset                   dscmz
076800010201     C                   move      vlrpoi        cmzdst
076900010201     C                   eval      cmzfld = vlrfou
077000010201     C                   eval      cmzmbd = vlrhdl
077100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077200021009     C***                if        prmfir = *blanks
077300021113     C                   eval      cmzfla = 'FIVAB00F'
077400021113     C                   eval      cmzmba = 'FIVAB00F'
077500021009     C***                else
077600021009     C***                eval      cmzfla = prmfir
077700021009     C***                eval      cmzmba = prmfir
077800021009     C***                endif
077900010201     C                   eval      cmznrr = *zeros
078000010201     C                   move      §ctrokvb      cmznrr
078100021111     C                   eval      cmzlba = vlrfl1
078200010201     C                   call(e)   'TIS711C'
078300010201     C                   parm                    dscmz
078400010201     C                   parm      *blanks       esito
078500010201     C                   if        %error
078600010201     C                             or cmzerr = '1'
078700010201     C                             or esito  = '1'
078800010201     C                   eval      wrkesito = '3'
078900010201     C                   endif
079000010205     C                   endif
079100150429     C                   endif
079200990920     C*
079300000613     C                   ENDSR
079400000613     C***
079500990910
079600000613     C     *inzsr        BEGSR
079700990910     C*
079800990910     C     *entry        plist
079900990920     C                   parm                    tivlrds
080000990921     C                   parm      wrkesito      esito
080100000724     C                   parm                    prmlit
080200000710     C                   parm                    prmfir
080300000613     C*
080400000830     C* CALCOLA LA DATA CORRENTE
080500000830     C                   time                    wn14             14 0
080600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080700000830     C                   z-add     wn8           g08dat
080800000830     C                   z-add     *zeros        g08inv
080900000830     C                   movel     '0'           g08err
081000000830     C                   call      'XSRDA8'
081100000830     C                   parm                    wlbda8
081200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
081300000830     C*
081400000613     C                   ENDSR
081500000613     C***
