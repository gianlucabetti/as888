000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVADWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVADwwr  O    E             DISK    usropn
000800010201     Ftis7lava  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000010201     Ftis7lavas O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010201     D*------------
001800010201     D* SCHIERE
001900010201     D*------------
002000021010     D skcdpvad        S             15    DIM(2000)
002100000801     D*----------------------------------------------------
002200000801     D* DICHIARAZIOINE VARIABILI DI WRK
002300000801     D*----------------------------------------------------
002400990920     D dscmz         e ds                  inz
002500990910     D psds           sds
002600990910     D  procname         *PROC
002700990920     D tivlrds       e ds                  extname(tivlr00f)
002800990910     D esito           s              1
002900000724     D prmlit          s             10
003000000710     D prmfir          s             10
003100990921     D wrkesito        s                   like(esito)
003200990915     D wrkdata         s               d
003300990915     D wrkora          s               t
003400000613     D rrnum           s              6  0 INZ(*zeros)
003500000621     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700010201     D depspe          s             16    INZ(*blanks)
003800010213     D depdat          s              8  0 INZ(*zeros)
003900010213     D depaas          s              4    INZ(*zeros)
004000010201     D curspe          s             16    INZ(*blanks)
004100010201     D depcdp          s             15    INZ(*blanks)
004200010305     D cntpez          s              5  0 INZ(*zeros)
004300010305     D cntcdp          s              5  0 INZ(*zeros)
004400010305     D depcnt          s              5  0 INZ(*zeros)
004500010201     D i               s              5i 0
004600010202     D parccm          s              8    INZ(*blanks)
004700010202     D parmbr          s             10    INZ(*blanks)
004800010202     D paropz          s              1    INZ(*blanks)
004900010202     D chkcall         s              1    INZ(*blanks)
005000010202     D s_VABCCM        s                   LIKE(VABCCM)
005100010202     D s_VABLNP        s                   LIKE(VABLNP)
005200010202     D s_VABAAS        s                   LIKE(VABAAS)
005300010202     D s_VABNRS        s                   LIKE(VABNRS)
005400010202     D s_VABNSP        s                   LIKE(VABNSP)
005500021113     D s_VABFGS        s                   LIKE(VABFGS)
005600000801
005700000801     D                 DS                  INZ
005800000801     D  ALFA35UNO              1     35
005900000801     D  ALFA35DUE             36     70
006000000801     D  ALFA70                 1     70
006100000801
006200010213     D                 DS                  INZ
006300010213     D  aaaa                   1      4  0
006400010213     D  mmgg                   5      8  0
006500010213     D  mm                     5      6  0
006600010213     D  gg                     7      8  0
006700010213     D  datawrk                1      8  0
006800010213
006900010213     D  Num8_0         s              8  0
007000000801     D  Num5_0         s              5  0
007100000830
007200000830     D*------------------
007300000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
007400000830     D*------------------
007500000830     D WLBDA8          DS                  INZ
007600000830     D  G08DAT                 1      8  0
007700000830     D  G08INV                 9     16  0
007800000830     D  G08ERR                17     17
007900000830     D  G08TGI                18     22  0
008000000830     D*
008100990908
008200010201
008300010201
008400990915     C                   time                    wrkdata
008500990915     C                   time                    wrkora
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900000724     C*
009000000724     C* SE OCCORRE SPEDIRE IN FILIALE
009100000724     C                   if        vlrpoi <> *zeros
009200000724     C*
009300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500000724     C     vlrpoi        chain     azorg01l
009600000724     C                   if        %found
009700000616     C                   movel(p)  CMD(1)        depcmd
009800020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009900000616     C*
010000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000616     C                   PARM                    depcmd
010400000616     C                   PARM                    LENGH
010500000724     C*
010600000724     C                   endif
010700000724     C                   endif
010800000616     C*
010900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011000000616     C                   movel(p)  CMD(2)        depcmd
011100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011200000616     C*
011300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    depcmd
011700000616     C                   PARM                    LENGH
011800000616     C*
011900010201     C                   if        not %open(tis7lavas)
012000010201     C                   open      tis7lavas
012100000616     C                   except    testdett
012200000613     C                   endif
012300000613     C*
012400010201     C                   EXSR      RWLAVA                                       LETT/SCR. VAB
012500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012600000613     C*
012700010202     C* Effettuo la chiamata al CLLE preposto
012800010202     C                   call(e)   'TIS7LAVC'
012900010202     C                   parm                    parccm
013000010202     C                   parm                    parmbr
013100010202     C                   parm      '2'           paropz
013200010202     C*
013300010201     C                   if        %open(tis7lavas)
013400000616     C                   except    findett
013500010201     C                   close     tis7lavas
013600000613     C                   endif
013700000616     C*
013800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013900011113     C                   Z-ADD     150           LENGH            15 5
014000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014100000616     C                   PARM                    CMD(3)
014200000616     C                   PARM                    LENGH
014300000616     C*
014400000801     C
014500010201     C                   seton                                        LR
014600000613
014700000613
014800000613     C*--------------------------------------------------------
014900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015000000613     C*--------------------------------------------------------
015100000621     C     STPR          BEGSR
015200000613     C*
015300010201     C                   if        not %open(tis7lava)
015400010201     C                   open      tis7lava
015500990915     C                   endif
015600990915     C*
015700990915     C                   except    riepilogo
015800990915     C*
015900010201     C                   if        %open(tis7lava)
016000010201     C                   close     tis7lava
016100990914     C                   endif
016200990910     C*
016300000613     C                   ENDSR
016400000613     C***
016500990908
016600000801
016700910830     C*--------------------------------------------------------
016800021113     C* RWLAVA  LEGGE tivin00r E SCRIVE FIVABWWR e FIVADWWR   *
016900910830     C*--------------------------------------------------------
017000010201     C     RWLAVA        BEGSR
017100990910     C*
017200990914     C                   if        not %open(tivin00r)
017300990908     C                   open      tivin00r
017400990914     C                   endif
017500021113     C                   if        not %open(fivabwwr)
017600021113     C                   open      fivabwwr
017700990914     C                   endif
017800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
017900010201     C                   exsr      prevad
018000010201     C*
018100010202     C                   if        chkcall = '0'
018200010202     C*
018300021113     C                   if        not %open(fivadwwr)
018400021113     C                   open      fivadwwr
018500010201     C                   endif
018600990910     C*
018700010201     C                   clear                   §CTROKVB          5 0
018800010201     C                   clear                   §CTROKVD          5 0
018900000801     C                   clear                   §CTRMO            5 0
019000000801     C                   clear                   §CTRNO            5 0
019100010201     C                   clear                   skcdpvad
019200990910     C*
019300921023     C                   DO        *HIVAL
019400990913     C*
019500990915     C                   READ      tivin00r                               70
019600000905     C                   if        vindta > *blanks
019700000613     C                   add       1             rrnum
019800000801     C*
019900000801     C                   if        *in70 = *off
020000000801     C                             and
020100000801     C                             (vinflg = *blanks
020200000801     C                              or vinflg = '0'
020300000801     C                              or vinflg = '2')
020400000801     C*
020500000801     C                   clear                   vinmsg
020600000801     C                   eval      vinflg = '1'
020700010305     C*
020800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
020900011115     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
021000010305     C                   MOVEL(p)  PiStr         curspe
021100010305     C*
021200010305     C                   if        depspe = *blanks                             => 1° giro
021300010305     C                   eval      depspe = curspe                              => memorizz. spediz
021400010305     C                   exsr      impvab
021500010305     C                   exsr      carvad
021600010305     C                   else
021700010305     C                   if        curspe <> depspe                             => rottura di spediz
021800010305     C                   eval      depspe = curspe                              => memorizz. spediz
021900010305     C                   exsr      scrivi
022000010305     C                   else                                                   => x stessa spediz
022100010305     C                   exsr      carvad                                       => carico VAD
022200010305     C                   endif
022300010305     C                   endif
022400010305     C                   endif
022500000905     C*
022600000905     C                   else
022700000905     C                   eval      vinflg = '1'
022800000905     C                   endif
022900000905     C*
023000000905     C  N70              update    tivin000
023100000905     C*
023200991022     C  N70              ENDdo
023300010202     C*
023400010305     C* Scarico i VAB e i VAD rimasti "in sospeso"
023500010305     C                   exsr      scrivi
023600010202     C*
023700010202     C                   endif
023800990910
023900990910     C* Se non ci sono record con errori ...
024000000710     C                   if        §ctrno = 0
024100990910     C* ... restituisco esito OK.
024200990921     C                   eval      wrkesito = '0'
024300990910     C                   else
024400010201     C                   if        §ctrokvb > 0
024500990921     C                   eval      wrkesito = '1'
024600000710     C                   else
024700000710     C                   eval      wrkesito = '2'
024800990910     C                   endif
024900000710     C                   endif
025000990910     C*
025100990914     C                   if        %open(tivin00r)
025200990908     C                   close     tivin00r
025300990914     C                   endif
025400021113     C                   if        %open(fivabwwr)
025500021113     C                   close     fivabwwr
025600990914     C                   endif
025700021113     C                   if        %open(fivadwwr)
025800021113     C                   close     fivadwwr
025900010201     C                   endif
026000990910     C*
026100010201     C                   if        §ctrokvb > 0
026200000724     C                             and vlrpoi <> *zeros
026300010202     C                   exsr      invio
026400990920     C                   endif
026500990920     C*
026600910830     C                   ENDSR
026700000613     C***
026800010305
026900010305     C*----------------------------------------------------*
027000010305     C*  SCARICAMENTO BUFFER RECORDS VAB/VAD
027100010305     C*----------------------------------------------------*
027200010305     C     SCRIVI        BEGSR
027300010305     C*
027400010305     C                   z-add     VABCCM        s_VABCCM
027500010305     C                   z-add     VABLNP        s_VABLNP
027600010305     C                   z-add     VABAAS        s_VABAAS
027700021113     C                   z-add     VABFGS        s_VABFGS
027800010305     C                   z-add     VABNRS        s_VABNRS
027900010305     C                   z-add     VABNSP        s_VABNSP
028000010305     C                   exsr      wrivad                                       => scarico i VAD
028100021113     C                   write     fivab000                                     => scarico il VAB
028200010305     C                   z-add     *zeros        cntpez
028300010305     C                   exsr      impvab                                       => ri-valorizzo VAB
028400010305     C                   exsr      carvad                                       => carico VAD
028500010305     C*
028600010305     C                   ENDSR
028700990920
028800000801     C*----------------------------------------------------*
028900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029000000801     C*----------------------------------------------------*
029100010201     C     INZVAR        BEGSR
029200000801     C*
029300000801     C                   MOVEL     *blanks       ALFA70
029400010201     C                   Z-ADD     *zeros        Num5_0
029500000801     C*
029600000801     C                   ENDSR
029700020326     C*----------------------------------------------------*
029800020326 xxx C*  IMPOSTAZIONE CAMPI COSTANTI
029900020326     C*----------------------------------------------------*
030000020326     C     DEFCAM        BEGSR
030100020326     C*
030200020326     C* Inizializzo il buffer del record da scrivere
030300021113     C                   CLEAR                   FIVAB000
030400020326     C* Imposto i valori di default...
030500020326     C                   EVAL      VABCCM = 0491999
030600020326     C                   EVAL      VADCCM = 0491999
030700020326     C                   EVAL      VABLNP = 049
030800020326     C                   EVAL      VADLNP = 049
030900020326     C                   EVAL      VABCTR = 000
031000020326     C* ... e poi verifico se sono stati passati come parametri
031100020326     C                   IF        vlrppt > *blanks
031200020326     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031300020326     C                   EXSR      CHKNUM
031400020326     C                   IF        PiInt=*on
031500020326     C                   Z-ADD     PiVal         VABCCM
031600020326     C                   Z-ADD     PiVal         VADCCM
031700020326     C                   ENDIF
031800020326     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031900020326     C                   EXSR      CHKNUM
032000020326     C                   IF        PiInt=*on
032100020326     C                   Z-ADD     PiVal         VABLNP
032200020326     C                   Z-ADD     PiVal         VADLNP
032300020326     C                   ENDIF
032400020326     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032500020326     C                   EXSR      CHKNUM
032600020326     C                   IF        PiInt=*on
032700020326     C                   Z-ADD     PiVal         VABCTR
032800020326     C                   ENDIF
032900020326     C                   ENDIF
033000020326     C*
033100020326     C                   ENDSR
033200000801     C*----------------------------------------------------*
033300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033400000801     C*----------------------------------------------------*
033500010201     C     IMPVAB        BEGSR
033600000801     C*
033700010305     C                   EXSR      INZVAR
033800010305     C                   EXSR      DEFCAM
033900010305     C*
034000000801     C                   Z-ADD     *zeros        errore            1 0
034100000830     C                   MOVEL     datcor        VABAAS
034200010201     C                   MOVEL     datcor        VADAAS
034300021113     C                   MOVE(P)   vlrpoi        VABFGS
034400021113     C                   MOVE(P)   vlrpoi        VADFGS
034500010213     C                   MOVEL     VABAAS        depaas
034600000830     C                   MOVE      datcor        VABMGS
034700010201     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
034800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035000020117     C* ==
035100010201     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035200010202     C                   EVAL      VABRMO=%trim(%subst(vindta:92:15))
035300010201     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
035400000801     C                   EXSR      CHKNUM
035500000801     C                   IF        PiInt=*on
035600000801     C                   Z-ADD     PiVal         Num5_0
035700010208     C                   MOVEL(p)  Num5_0        VABCAD
035800000801     C                   ELSE
035900000801     C                   ADD       1             errore
036000000801     C                   EVAL      vinmsg = %trimr(vinmsg)
036100000801     C                             + ' ' + 'VABCAD'
036200000801     C                   ENDIF
036300010201     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
036400010201     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036500010201     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
036600010201     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
036700010201     C                   EXSR      CHKNUM
036800010201     C                   IF        PiInt=*on
036900010201     C                   Z-ADD     PiVal         Num5_0
037000010208     C                   MOVE      Num5_0        VABNCL
037100010201     C                   ELSE
037200010201     C                   ADD       1             errore
037300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037400010201     C                             + ' ' + 'VABNCL'
037500010201     C                   ENDIF
037600010201     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
037700010201     C                   EXSR      CHKNUM
037800010201     C                   IF        PiNum=*on
037900011115     C                   Z-ADD     PiVal         VABPKB
038000010201     C                   ELSE
038100010201     C                   ADD       1             errore
038200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038300010201     C                             + ' ' + 'VABPKB'
038400010201     C                   ENDIF
038500010201     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
038600010201     C                   EXSR      CHKNUM
038700010201     C                   IF        PiInt=*on
038800011115     C                   Z-ADD     PiVal         VABRMN
038900010201     C                   ELSE
039000010201     C                   ADD       1             errore
039100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039200010201     C                             + ' ' + 'VABRMN'
039300010201     C                   ENDIF
039400010201     C                   EVAL      ALFA70=%trim(%subst(vindta:197:80))
039500010201     C                   MOVEL     ALFA35UNO     VABNOT
039600010201     C                   MOVEL     ALFA35DUE     VABNT2
039700011115     C*
039800011115     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
039900011115     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
040000010201     C                   EXSR      CHKNUM
040100010201     C                   IF        PiNum=*on
040200011115     C                   Z-ADD     PiVal         VABCAS
040300030509     C                   IF        VABCAS > *zeros
040400030506     C                   EVAL      VABTIC = 'BM'
040500030509     C                   ENDIF
040600010201     C                   ELSE
040700010201     C                   ADD       1             errore
040800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040900010201     C                             + ' ' + 'VABCAS'
041000010201     C                   ENDIF
041100011115     C
041200011115     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
041300011115     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
041400010201     C                   EXSR      CHKNUM
041500010201     C                   IF        PiNum=*on
041600011115     C                   Z-ADD     PiVal         VABIAS
041700010201     C                   ELSE
041800010201     C                   ADD       1             errore
041900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042000010201     C                             + ' ' + 'VABIAS'
042100010201     C                   ENDIF
042200011115     C*
042300011115     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
042400010202     C                   IF        PiStr>*blanks
042500010201     C                   EXSR      CHKNUM
042600010201     C                   IF        PiNum=*on
042700011115     C                   Z-ADD     PiVal         VABVLB
042800010201     C                   ELSE
042900010201     C                   ADD       1             errore
043000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
043100010201     C                             + ' ' + 'VABVLB'
043200010201     C                   ENDIF
043300010202     C                   ENDIF
043400011115     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
043500010201     C                   EXSR      CHKNUM
043600010201     C                   IF        PiInt=*on
043700011115     C                   Z-ADD     PiVal         VABNSP
043800011115     C                   Z-ADD     PiVal         VADNSP
043900010201     C                   ELSE
044000010201     C                   ADD       1             errore
044100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
044200010201     C                             + ' ' + 'VABNSP'
044300010201     C                   ENDIF
044400010213     C* AGGIUNTA: gestione data consegna richiesta
044500010213     C                   EVAL      PiStr=depaas + %trim(%subst(vindta:203:2))+
044600010213     C                                            %trim(%subst(vindta:200:2))
044700010213     C                   EXSR      CHKNUM
044800010213     C                   IF        PiInt=*on
044900010213     C                   Z-ADD     PiVal         Num8_0
045000010213     C                   MOVEL(p)  Num8_0        depdat
045100010219     C                   z-add     depdat        datawrk
045200010213     C                   IF        depdat < datcor
045300010214     C                   add       1             aaaa
045400010219     C                   ENDIF
045500010214     C* A questo punto eseguo controllo di validità della data così ottenuta
045600010214     C                   z-add     *zeros        G08DAT
045700010214     C                   z-add     datawrk       G08INV
045800010214     C                   movel     '3'           G08ERR
045900010214     C                   call      'XSRDA8'
046000010214     C                   parm                    WLBDA8
046100010214     C     G08ERR        ifeq      '1'
046200010214     C                   else
046300010213     C                   z-add     datawrk       VABDCR
046400091126     C                   eval      VABTCR = 'P'
046500010214     C                   endif
046600010214     C                   ENDIF
046700010205     C*
046800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
046900010205     C                   IF        VABCAS > *zeros
047000010205     C                   EVAL      VABCBO = '4'
047100010205     C                   ELSE
047200010205     C                   EVAL      VABCBO = '1'
047300010205     C                   ENDIF
047400010205     C*
047500010205     C                   EVAL      VABCTM = 'DT'
047600011113     C*
047700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
047800011113     C                   EXSR      CHKIMPDIV
047900010202     C*
048000000801     C* Ebbene...
048100000801     C                   ADD       1             §CTRMO
048200010201     C                   IF        errore <> *zeros
048300000801     C                   ADD       1             §CTRNO
048400000801     C                   EVAL      recko = vindta
048500000801     C                   EXCEPT    rigadett
048600000801     C                   EVAL      vinflg = '2'
048700000801     C                   ELSE
048800010201     C                   ADD       1             §CTROKVB
048900000801     C                   ENDIF
049000000801     C*
049100000801     C                   ENDSR
049200010201     C*----------------------------------------------------*
049300021113     C*  CARICAMENTO CAMPI DA FLAT FILE (X FIVAD)
049400010201     C*----------------------------------------------------*
049500010201     C     CARVAD        BEGSR
049600010201     C*
049700010201     C* Carico la schiera dei codici prodotto per cliente/spedizione
049800010201     C                   add       1             i
049900041130     C                   eval      skcdpvad(i)=%subst(vindta:331:8)
050000010201     C*
050100010201     C                   ENDSR
050200010201     C*----------------------------------------------------*
050300021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAD)
050400010201     C*----------------------------------------------------*
050500010305     C     WRIVAD        BEGSR
050600010201     C*
050700010201     C* Ordino la schera
050800010201     C                   sorta     skcdpvad
050900010201     C*
051000010201     C                   z-add     1             i
051100010201     C                   z-add     *zeros        cntcdp
051200010201     C* Mi posiziono sul primo elemento pieno della schiera e porto avanti l'indice
051300010202     C                   dow       skcdpvad(i) = *blanks and
051400021010     C                             i < 2000
051500010201     C                   add       1             i
051600010201     C                   enddo
051700010202     C*
051800021010     C                   if        i <= 2000
051900010201     C* Mi salvo il primo codice prodotto
052000041130     C                   eval      depcdp=%subst(skcdpvad(i):1:8)
052100010305     C* Inizio il ciclo principale
052200021010     C                   dow       i <= 2000
052300010305     C                   z-add     1             depcnt
052400010305     C                   eval      cntpez = cntpez + depcnt
052500010201     C* Effettuo considerazioni sulla totalizzazione del dettaglio colli per codice prodotto
052600041130     C                   if        depcdp = %subst(skcdpvad(i):1:8)
052700010305     C                   add       depcnt        cntcdp
052800010201     C                   else
052900021113     C                   clear                   FIVAD000
053000021113     C* Valorizzo l buffer di scrittura del FIVAD
053100010201     C                   eval      VADCCM = s_VABCCM
053200010201     C                   eval      VADLNP = s_VABLNP
053300010201     C                   eval      VADAAS = s_VABAAS
053400021113     C                   eval      VADFGS = s_VABFGS
053500010201     C                   eval      VADNRS = s_VABNRS
053600010201     C                   eval      VADNSP = s_VABNSP
053700010201     C                   eval      VADCDP = depcdp
053800010201     C                   eval      VADNCL = cntcdp
053900021113     C                   write     FIVAD000
054000041130     C                   eval      depcdp=%subst(skcdpvad(i):1:8)
054100010305     C                   z-add     depcnt        cntcdp
054200010201     C                   add       1             §CTROKVD
054300010201     C                   endif
054400010201     C                   add       1             i
054500010201     C                   enddo
054600010305     C*
054700010305     C* A questo punto so qunti sono i colli effettivi della testata della bolla
054800010305     C                   z-add     cntpez        VABNCL
054900010202     C*
055000010305     C* Quando esco dal ciclo scarico gli ultimi codici prodotto rimasti in "sospeso"
055100010202     C                   eval      VADCCM = s_VABCCM
055200010202     C                   eval      VADLNP = s_VABLNP
055300010202     C                   eval      VADAAS = s_VABAAS
055400021113     C                   eval      VADFGS = s_VABFGS
055500010202     C                   eval      VADNRS = s_VABNRS
055600010202     C                   eval      VADNSP = s_VABNSP
055700010202     C                   eval      VADCDP = depcdp
055800010202     C                   eval      VADNCL = cntcdp
055900021113     C                   write     FIVAD000
056000010202     C                   add       1             §CTROKVD
056100010202     C*
056200010202     C                   endif
056300010202     C*
056400010202     C* Re-inizializzo la schiera ed il suo indice
056500010202     C                   z-add     *zeros        i
056600010202     C                   clear                   skcdpvad
056700010201     C*
056800010201     C                   ENDSR
056900010202     C*----------------------------------------------------*
057000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR
057100010202     C*----------------------------------------------------*
057200010202     C     PREVAD        BEGSR
057300010202     C*
057400021113     C* Compongo il nome del membro da dare al FIVADWWR
057500010202     C                   eval      parmbr = vlrhdl
057600010202     C                   movel     'M'           parmbr
057700010203     C                   eval      parccm = '00491999'
057800010202     C                   eval      paropz = '1'
057900010202     C* Effettuo la chiamata al CLLE preposto
058000010202     C                   call(e)   'TIS7LAVC'
058100010202     C                   parm                    parccm
058200010202     C                   parm                    parmbr
058300010202     C                   parm                    paropz
058400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058500010202     C                   if        %error
058600010202     C                   movel     '1'           chkcall
058700010202     C                   else
058800010202     C                   movel     '0'           chkcall
058900010202     C                   endif
059000010202     C*
059100010202     C                   ENDSR
059200000801     C*----------------------------------------------------*
059300000801     C*  CONTROLLO NUMERICITA' CAMPI
059400000801     C*----------------------------------------------------*
059500000801     C     CHKNUM        BEGSR
059600000801     C*
059700000801     C                   call(e)   'ISNUMERIC'
059800000801     C                   PARM                    PiStr            30
059900000801     C                   PARM      ','           PiDecChr          1
060000000801     C                   PARM      *ZEROS        PiVal            30 9
060100000801     C                   PARM      '0'           PiInt             1
060200000801     C                   PARM      '0'           PiNum             1
060300000801     C                   IF        %error
060400000801     C                   EVAL      PiInt=*off
060500000801     C                   ENDIF
060600000801     C*
060700000801     C                   ENDSR
060800000801     C***
060900000801
061000011113
061100011113     C*----------------------------------------------------*
061200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061300011113     C*----------------------------------------------------*
061400011113     C     CHKIMPDIV     BEGSR
061500011113     C*
061600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061700011113     C                   Z-ADD     *zeros        wrkDec            9 9
061800011113     C*
061900011113     C* Come prima cosa effettuo considerazioni sulla divisa
062000011113     C                   IF        vabIAS > *zeros
062100011113     C                   IF        vabVAS <> 'EUR'
062200011113     C                   EVAL      vabVAS =  'ITL'
062300011113     C                   ENDIF
062400011113     C                   ENDIF
062500011113     C*
062600011113     C                   IF        vabCAS > *zeros
062700011113     C                   IF        vabVCA <> 'EUR'
062800011113     C                   EVAL      vabVCA =  'ITL'
062900011113     C                   ENDIF
063000011113     C                   ENDIF
063100011113     C*
063200011113     C                   IF        vabVMD > *zeros
063300011113     C                   IF        vabVAD <> 'EUR'
063400011113     C                   EVAL      vabVAD =  'ITL'
063500011113     C                   ENDIF
063600011113     C                   ENDIF
063700011113     C*
063800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063900011113     C                   Z-ADD     vabIAS        wrkDec
064000011113     C                   IF        wrkDec > *zeros
064100011113     C                   IF        vabVAS = 'ITL'
064200011113     C                   EVAL      vabIAS = *zeros
064300011113     C                   ENDIF
064400011113     C                   ENDIF
064500011113     C*
064600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064700011113     C                   Z-ADD     vabCAS        wrkDec
064800011113     C                   IF        wrkDec > *zeros
064900011113     C                   IF        vabVCA = 'ITL'
065000011113     C                   EVAL      vabCAS = *zeros
065100011113     C                   ENDIF
065200011113     C                   ENDIF
065300011113     C*
065400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065500011113     C                   Z-ADD     vabVMD        wrkDec
065600011113     C                   IF        wrkDec > *zeros
065700011113     C                   IF        vabVAD = 'ITL'
065800011113     C                   EVAL      vabVMD = *zeros
065900011113     C                   ENDIF
066000011113     C                   ENDIF
066100011113     C*
066200011113     C                   ENDSR
066300011113     C***
066400011113
066500011113
066600000801
066700000801
066800990920      /TITLE Invio dei dati al punto operativo.
066900010202     C     invio         BEGSR
067000990920     C*
067100021113     C* 1° invio FIVAD
067200010201     C                   reset                   dscmz
067300010201     C                   move      vlrpoi        cmzdst
067400021113     C                   eval      cmzfld = 'FIVADWWR'
067500010201     C                   eval      cmzmbd = vlrhdl
067600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067700021009     C***                if        prmfir = *blanks
067800021113     C                   eval      cmzfla = 'FIVAD00F'
067900021113     C                   eval      cmzmba = 'FIVAD00F'
068000021009     C***                else
068100021009     C***                eval      cmzfla = prmfir
068200021009     C***                eval      cmzmba = prmfir
068300021009     C***                endif
068400010201     C                   eval      cmznrr = *zeros
068500010201     C                   move      §ctrokvd      cmznrr
068600021111     C                   eval      cmzlba = vlrfl1
068700010201     C                   call(e)   'TIS711C'
068800010201     C                   parm                    dscmz
068900010201     C                   parm      *blanks       esito
069000010205     C                   if        %error
069100010205     C                             or cmzerr = '1'
069200010205     C                             or esito  = '1'
069300010205     C                   eval      wrkesito = '3'
069400010205     C                   else
069500010201     C*
069600021113     C* 2° invio FIVAB
069700010201     C                   reset                   dscmz
069800010201     C                   move      vlrpoi        cmzdst
069900010201     C                   eval      cmzfld = vlrfou
070000010201     C                   eval      cmzmbd = vlrhdl
070100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070200021009     C***                if        prmfir = *blanks
070300021113     C                   eval      cmzfla = 'FIVAB00F'
070400021113     C                   eval      cmzmba = 'FIVAB00F'
070500021009     C***                else
070600021009     C***                eval      cmzfla = prmfir
070700021009     C***                eval      cmzmba = prmfir
070800021009     C***                endif
070900010201     C                   eval      cmznrr = *zeros
071000010201     C                   move      §ctrokvb      cmznrr
071100021111     C                   eval      cmzlba = vlrfl1
071200010201     C                   call(e)   'TIS711C'
071300010201     C                   parm                    dscmz
071400010201     C                   parm      *blanks       esito
071500010201     C                   if        %error
071600010201     C                             or cmzerr = '1'
071700010201     C                             or esito  = '1'
071800010201     C                   eval      wrkesito = '3'
071900010201     C                   endif
072000010205     C                   endif
072100990920     C*
072200000613     C                   ENDSR
072300000613     C***
072400990910
072500000613     C     *inzsr        BEGSR
072600990910     C*
072700990910     C     *entry        plist
072800990920     C                   parm                    tivlrds
072900990921     C                   parm      wrkesito      esito
073000000724     C                   parm                    prmlit
073100000710     C                   parm                    prmfir
073200000613     C*
073300000830     C* CALCOLA LA DATA CORRENTE
073400000830     C                   time                    wn14             14 0
073500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
073600000830     C                   z-add     wn8           g08dat
073700000830     C                   z-add     *zeros        g08inv
073800000830     C                   movel     '0'           g08err
073900000830     C                   call      'XSRDA8'
074000000830     C                   parm                    wlbda8
074100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
074200000830     C*
074300000613     C                   ENDSR
074400000613     C***
074500990908
074600010201     Otis7lava  E            riepilogo         2
074700990915     O                                              'Upload via Internet'
074800990915     O                                           +1 'Traduzione TIVIN00R -'
074900021113     O                                           +1 'FIVABWWR/FIVADWWR'
075000010201     O                                           +1 'Report testate bolle'
075100990915     O          E            riepilogo   2
075200990915     O                       wrkdata
075300990915     O                       wrkora              +1
075400990915     O                       procname            +1
075500990915     O          E            riepilogo   2
075600990915     O                                              'Cliente..................:'
075700990915     O                       VABCCM        z     +1
075800990915     O          E            riepilogo   2
075900990920     O                                              'Riferimento Strategi.....:'
076000990920     O                       vlrhdl              +1
076100990915     O          E            riepilogo   2
076200990915     O                                              'Giusti...................:'
076300010201     O                       §CTROKVB      2   +  1
076400990915     O          E            riepilogo   2
076500990915     O                                              'Sbagliati e corretti.....:'
076600971022     O                       §CTRMO        2   +  1
076700990915     O          E            riepilogo   2
076800990915     O                                              'Sbagliati e scartati.....:'
076900971022     O                       §CTRNO        2   +  1
077000000613
077100010201     Otis7lavas E            testdett          2
077200000613     O                                              'Upload via Internet'
077300000613     O                                           +1 'Traduzione TIVIN00R -'
077400021113     O                                           +1 'FIVABWWR/FIVADWWR'
077500010201     O                                           +1 'Report testate bolle'
077600000616     O          E            testdett    3
077700000613     O                                           +2 'N° rec'
077800000613     O                                           +3 'Anteprima contenuto'
077900000616     O          E            rigadett    2
078000000613     O                       rrnum               +2
078100000621     O                       recko               +3
078200000616     O          E            findett     2
078300000613     O                       wrkdata
078400000613     O                       wrkora              +1
078500000613     O                       procname            +1
078600000616     O          E            findett     2
078700000613     O                                              'Cliente..................:'
078800000613     O                       VABCCM        z     +1
078900000616     O          E            findett     2
079000000613     O                                              'Riferimento Strategi.....:'
079100000613     O                       vlrhdl              +1
079200000616     O          E            findett     2
079300000613     O                                              'Giusti...................:'
079400010201     O                       §CTROKVB      2   +  1
079500000616     O          E            findett     2
079600000613     O                                              'Sbagliati e corretti.....:'
079700000613     O                       §CTRMO        2   +  1
079800000616     O          E            findett     2
079900000613     O                                              'Sbagliati e scartati.....:'
080000000613     O                       §CTRNO        2   +  1
080100000512** CMD - COMANDI CL
080200011113OVRPRTF FILE(TIS7LAVA) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
080300011113OVRPRTF FILE(TIS7LAVAS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
080400010201DLTOVR FILE(TIS7LAVA TIS7LAVAS) LVL(*)
080500000512
080600000512
