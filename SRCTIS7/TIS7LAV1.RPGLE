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
005401021113     D s_VABFGS        s                   LIKE(VABFGS)
005500000801
005600000801     D                 DS                  INZ
005700000801     D  ALFA35UNO              1     35
005800000801     D  ALFA35DUE             36     70
005900000801     D  ALFA70                 1     70
006000000801
006100010213     D                 DS                  INZ
006200010213     D  aaaa                   1      4  0
006300010213     D  mmgg                   5      8  0
006400010213     D  mm                     5      6  0
006500010213     D  gg                     7      8  0
006600010213     D  datawrk                1      8  0
006700010213
006800010208     D  Num15_0        s             15  0
006900010208     D  Num13_3        s             13  3
007000010213     D  Num8_0         s              8  0
007100010207     D  Num7_1         s              7  1
007200010208     D  Num7_0         s              7  0
007300010207     D  Num5_3         s              5  3
007400000801     D  Num5_0         s              5  0
007500000830
007600000830     D*------------------
007700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
007800000830     D*------------------
007900000830     D WLBDA8          DS                  INZ
008000000830     D  G08DAT                 1      8  0
008100000830     D  G08INV                 9     16  0
008200000830     D  G08ERR                17     17
008300000830     D  G08TGI                18     22  0
008400000830     D*
008500990908
008600010201
008700010201
008800990915     C                   time                    wrkdata
008900990915     C                   time                    wrkora
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300000724     C*
009400000724     C* SE OCCORRE SPEDIRE IN FILIALE
009500000724     C                   if        vlrpoi <> *zeros
009600000724     C*
009700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009900000724     C     vlrpoi        chain     azorg01l
010000000724     C                   if        %found
010100000616     C                   movel(p)  CMD(1)        depcmd
010200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010300000616     C*
010400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010500011113     C                   Z-ADD     150           LENGH            15 5
010600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000616     C                   PARM                    depcmd
010800000616     C                   PARM                    LENGH
010900000724     C*
011000000724     C                   endif
011100000724     C                   endif
011200000616     C*
011300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011400000616     C                   movel(p)  CMD(2)        depcmd
011500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011600000616     C*
011700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    depcmd
012100000616     C                   PARM                    LENGH
012200000616     C*
012300010201     C                   if        not %open(tis7lavas)
012400010201     C                   open      tis7lavas
012500000616     C                   except    testdett
012600000613     C                   endif
012700000613     C*
012800010201     C                   EXSR      RWLAVA                                       LETT/SCR. VAB
012900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013000000613     C*
013100010202     C* Effettuo la chiamata al CLLE preposto
013200010202     C                   call(e)   'TIS7LAVC'
013300010202     C                   parm                    parccm
013400010202     C                   parm                    parmbr
013500010202     C                   parm      '2'           paropz
013600010202     C*
013700010201     C                   if        %open(tis7lavas)
013800000616     C                   except    findett
013900010201     C                   close     tis7lavas
014000000613     C                   endif
014100000616     C*
014200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000616     C                   PARM                    CMD(3)
014600000616     C                   PARM                    LENGH
014700000616     C*
014800000801     C
014900010201     C                   seton                                        LR
015000000613
015100000613
015200000613     C*--------------------------------------------------------
015300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015400000613     C*--------------------------------------------------------
015500000621     C     STPR          BEGSR
015600000613     C*
015700010201     C                   if        not %open(tis7lava)
015800010201     C                   open      tis7lava
015900990915     C                   endif
016000990915     C*
016100990915     C                   except    riepilogo
016200990915     C*
016300010201     C                   if        %open(tis7lava)
016400010201     C                   close     tis7lava
016500990914     C                   endif
016600990910     C*
016700000613     C                   ENDSR
016800000613     C***
016900990908
017000000801
017100910830     C*--------------------------------------------------------
017200021113     C* RWLAVA  LEGGE tivin00r E SCRIVE FIVABWWR e FIVADWWR   *
017300910830     C*--------------------------------------------------------
017400010201     C     RWLAVA        BEGSR
017500990910     C*
017600990914     C                   if        not %open(tivin00r)
017700990908     C                   open      tivin00r
017800990914     C                   endif
017900021113     C                   if        not %open(fivabwwr)
018000021113     C                   open      fivabwwr
018100990914     C                   endif
018200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
018300010201     C                   exsr      prevad
018400010201     C*
018500010202     C                   if        chkcall = '0'
018600010202     C*
018700021113     C                   if        not %open(fivadwwr)
018800021113     C                   open      fivadwwr
018900010201     C                   endif
019000990910     C*
019100010201     C                   clear                   §CTROKVB          5 0
019200010201     C                   clear                   §CTROKVD          5 0
019300000801     C                   clear                   §CTRMO            5 0
019400000801     C                   clear                   §CTRNO            5 0
019500010201     C                   clear                   skcdpvad
019600990910     C*
019700921023     C                   DO        *HIVAL
019800990913     C*
019900990915     C                   READ      tivin00r                               70
020000000905     C                   if        vindta > *blanks
020100000613     C                   add       1             rrnum
020200000801     C*
020300000801     C                   if        *in70 = *off
020400000801     C                             and
020500000801     C                             (vinflg = *blanks
020600000801     C                              or vinflg = '0'
020700000801     C                              or vinflg = '2')
020800000801     C*
020900000801     C                   clear                   vinmsg
021000000801     C                   eval      vinflg = '1'
021100010305     C*
021200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
021300010305     C                   EVAL      PiStr=%trim(%subst(vindta:303:16))
021400010305     C                   MOVEL(p)  PiStr         curspe
021500010305     C*
021600010305     C                   if        depspe = *blanks                             => 1° giro
021700010305     C                   eval      depspe = curspe                              => memorizz. spediz
021800010305     C                   exsr      impvab
021900010305     C                   exsr      carvad
022000010305     C                   else
022100010305     C                   if        curspe <> depspe                             => rottura di spediz
022200010305     C                   eval      depspe = curspe                              => memorizz. spediz
022300010305     C                   exsr      scrivi
022400010305     C                   else                                                   => x stessa spediz
022500010305     C                   exsr      carvad                                       => carico VAD
022600010305     C                   endif
022700010305     C                   endif
022800010305     C                   endif
022900000905     C*
023000000905     C                   else
023100000905     C                   eval      vinflg = '1'
023200000905     C                   endif
023300000905     C*
023400000905     C  N70              update    tivin000
023500000905     C*
023600991022     C  N70              ENDdo
023700010202     C*
023800010305     C* Scarico i VAB e i VAD rimasti "in sospeso"
023900010305     C                   exsr      scrivi
024000010202     C*
024100010202     C                   endif
024200990910
024300990910     C* Se non ci sono record con errori ...
024400000710     C                   if        §ctrno = 0
024500990910     C* ... restituisco esito OK.
024600990921     C                   eval      wrkesito = '0'
024700990910     C                   else
024800010201     C                   if        §ctrokvb > 0
024900990921     C                   eval      wrkesito = '1'
025000000710     C                   else
025100000710     C                   eval      wrkesito = '2'
025200990910     C                   endif
025300000710     C                   endif
025400990910     C*
025500990914     C                   if        %open(tivin00r)
025600990908     C                   close     tivin00r
025700990914     C                   endif
025800021113     C                   if        %open(fivabwwr)
025900021113     C                   close     fivabwwr
026000990914     C                   endif
026100021113     C                   if        %open(fivadwwr)
026200021113     C                   close     fivadwwr
026300010201     C                   endif
026400990910     C*
026500010201     C                   if        §ctrokvb > 0
026600000724     C                             and vlrpoi <> *zeros
026700010202     C                   exsr      invio
026800990920     C                   endif
026900990920     C*
027000910830     C                   ENDSR
027100000613     C***
027200010305
027300010305     C*----------------------------------------------------*
027400010305     C*  SCARICAMENTO BUFFER RECORDS VAB/VAD
027500010305     C*----------------------------------------------------*
027600010305     C     SCRIVI        BEGSR
027700010305     C*
027800010305     C                   z-add     VABCCM        s_VABCCM
027900010305     C                   z-add     VABLNP        s_VABLNP
028000010305     C                   z-add     VABAAS        s_VABAAS
028001021113     C                   z-add     VABFGS        s_VABFGS
028100010305     C                   z-add     VABNRS        s_VABNRS
028200010305     C                   z-add     VABNSP        s_VABNSP
028300010305     C                   exsr      wrivad                                       => scarico i VAD
028400021113     C                   write     fivab000                                     => scarico il VAB
028500010305     C                   z-add     *zeros        cntpez
028600010305     C                   exsr      impvab                                       => ri-valorizzo VAB
028700010305     C                   exsr      carvad                                       => carico VAD
028800010305     C*
028900010305     C                   ENDSR
029000990920
029100000801     C*----------------------------------------------------*
029200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029300000801     C*----------------------------------------------------*
029400010201     C     INZVAR        BEGSR
029500000801     C*
029600000801     C                   MOVEL     *blanks       ALFA70
029700010208     C                   Z-ADD     *zeros        Num15_0
029800010208     C                   Z-ADD     *zeros        Num13_3
029900010207     C                   Z-ADD     *zeros        Num7_1
030000010208     C                   Z-ADD     *zeros        Num7_0
030100010207     C                   Z-ADD     *zeros        Num5_3
030200010201     C                   Z-ADD     *zeros        Num5_0
030300000801     C*
030400000801     C                   ENDSR
030500000801     C*----------------------------------------------------*
030600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030700000801     C*----------------------------------------------------*
030800000801     C     DEFCAM        BEGSR
030900000801     C*
031000021113     C                   CLEAR                   FIVAB000
031100010208  ==>C                   Z-ADD     0491999       VABCCM
031200010208  ==>C                   Z-ADD     0491999       VADCCM
031300000801     C                   Z-ADD     049           VABLNP
031400010201     C                   Z-ADD     049           VADLNP
031500010201     C                   Z-ADD     000           VABCTR
031600000801     C*
031700000801     C                   ENDSR
031800000801     C*----------------------------------------------------*
031900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032000000801     C*----------------------------------------------------*
032100010201     C     IMPVAB        BEGSR
032200000801     C*
032300010305     C                   EXSR      INZVAR
032400010305     C                   EXSR      DEFCAM
032500010305     C*
032600000801     C                   Z-ADD     *zeros        errore            1 0
032700000830     C                   MOVEL     datcor        VABAAS
032800010201     C                   MOVEL     datcor        VADAAS
032900010213     C                   MOVEL     VABAAS        depaas
033000000830     C                   MOVE      datcor        VABMGS
033001021113     C                   MOVE(P)   vlrpoi        VABFGS
033002021113     C                   MOVE(P)   vlrpoi        VADFGS
033100010201     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
033200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033400020117     C* ==
033500010201     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
033600010202     C                   EVAL      VABRMO=%trim(%subst(vindta:92:15))
033700010201     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033800000801     C                   EXSR      CHKNUM
033900000801     C                   IF        PiInt=*on
034000000801     C                   Z-ADD     PiVal         Num5_0
034100010208     C                   MOVEL(p)  Num5_0        VABCAD
034200000801     C                   ELSE
034300000801     C                   ADD       1             errore
034400000801     C                   EVAL      vinmsg = %trimr(vinmsg)
034500000801     C                             + ' ' + 'VABCAD'
034600000801     C                   ENDIF
034700010201     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
034800010201     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
034900010201     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
035000010201     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035100010201     C                   EXSR      CHKNUM
035200010201     C                   IF        PiInt=*on
035300010201     C                   Z-ADD     PiVal         Num5_0
035400010208     C                   MOVE      Num5_0        VABNCL
035500010201     C                   ELSE
035600010201     C                   ADD       1             errore
035700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035800010201     C                             + ' ' + 'VABNCL'
035900010201     C                   ENDIF
036000010201     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
036100010201     C                   EXSR      CHKNUM
036200010201     C                   IF        PiNum=*on
036300010207     C                   Z-ADD     PiVal         Num7_1
036400010208     C                   MOVE      Num7_1        VABPKB
036500010201     C                   ELSE
036600010201     C                   ADD       1             errore
036700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036800010201     C                             + ' ' + 'VABPKB'
036900010201     C                   ENDIF
037000010201     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
037100010201     C                   EXSR      CHKNUM
037200010201     C                   IF        PiInt=*on
037300010208     C                   Z-ADD     PiVal         Num15_0
037400010208     C                   MOVE      Num15_0       VABRMN
037500010201     C                   ELSE
037600010201     C                   ADD       1             errore
037700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037800010201     C                             + ' ' + 'VABRMN'
037900010201     C                   ENDIF
038000010201     C                   EVAL      ALFA70=%trim(%subst(vindta:197:80))
038100010201     C                   MOVEL     ALFA35UNO     VABNOT
038200010201     C                   MOVEL     ALFA35DUE     VABNT2
038300010201     C                   EVAL      PiStr=%trim(%subst(vindta:277:9))
038400010201     C                   EXSR      CHKNUM
038500010201     C                   IF        PiNum=*on
038600010208     C                   Z-ADD     PiVal         Num13_3
038700010208     C                   MOVE      Num13_3       VABCAS
038800010201     C                   ELSE
038900010201     C                   ADD       1             errore
039000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039100010201     C                             + ' ' + 'VABCAS'
039200010201     C                   ENDIF
039300010201     C                   EVAL      PiStr=%trim(%subst(vindta:286:9))
039400010201     C                   EXSR      CHKNUM
039500010201     C                   IF        PiNum=*on
039600010208     C                   Z-ADD     PiVal         Num13_3
039700010208     C                   MOVE      Num13_3       VABIAS
039800010201     C                   ELSE
039900010201     C                   ADD       1             errore
040000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040100010201     C                             + ' ' + 'VABIAS'
040200010201     C                   ENDIF
040300010201     C                   EVAL      PiStr=%trim(%subst(vindta:296:7))
040400010202     C                   IF        PiStr>*blanks
040500010201     C                   EXSR      CHKNUM
040600010201     C                   IF        PiNum=*on
040700010207     C                   Z-ADD     PiVal         Num5_3
040800010208     C                   MOVE      Num5_3        VABVLB
040900010201     C                   ELSE
041000010201     C                   ADD       1             errore
041100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
041200010201     C                             + ' ' + 'VABVLB'
041300010201     C                   ENDIF
041400010202     C                   ENDIF
041500010201     C                   EVAL      PiStr=%trim(%subst(vindta:303:16))
041600010201     C                   EXSR      CHKNUM
041700010201     C                   IF        PiInt=*on
041800010208     C                   Z-ADD     PiVal         Num7_0
041900010208     C                   MOVE      Num7_0        VABNSP
042000010208     C                   MOVE      Num7_0        VADNSP
042100010201     C                   ELSE
042200010201     C                   ADD       1             errore
042300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042400010201     C                             + ' ' + 'VABNSP'
042500010201     C                   ENDIF
042600010213     C* AGGIUNTA: gestione data consegna richiesta
042700010213     C                   EVAL      PiStr=depaas + %trim(%subst(vindta:203:2))+
042800010213     C                                            %trim(%subst(vindta:200:2))
042900010213     C                   EXSR      CHKNUM
043000010213     C                   IF        PiInt=*on
043100010213     C                   Z-ADD     PiVal         Num8_0
043200010213     C                   MOVEL(p)  Num8_0        depdat
043300010219     C                   z-add     depdat        datawrk
043400010213     C                   IF        depdat < datcor
043500010214     C                   add       1             aaaa
043600010219     C                   ENDIF
043700010214     C* A questo punto eseguo controllo di validità della data così ottenuta
043800010214     C                   z-add     *zeros        G08DAT
043900010214     C                   z-add     datawrk       G08INV
044000010214     C                   movel     '3'           G08ERR
044100010214     C                   call      'XSRDA8'
044200010214     C                   parm                    WLBDA8
044300010214     C     G08ERR        ifeq      '1'
044400010214     C                   else
044500010213     C                   z-add     datawrk       VABDCR
044600010214     C                   endif
044700010214     C                   ENDIF
044800010205     C*
044900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045000010205     C                   IF        VABCAS > *zeros
045100010205     C                   EVAL      VABCBO = '4'
045200010205     C                   ELSE
045300010205     C                   EVAL      VABCBO = '1'
045400010205     C                   ENDIF
045500010205     C*
045600010205     C                   EVAL      VABCTM = 'DT'
045700011113     C*
045800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045900011113     C                   EXSR      CHKIMPDIV
046000010202     C*
046100000801     C* Ebbene...
046200000801     C                   ADD       1             §CTRMO
046300010201     C                   IF        errore <> *zeros
046400000801     C                   ADD       1             §CTRNO
046500000801     C                   EVAL      recko = vindta
046600000801     C                   EXCEPT    rigadett
046700000801     C                   EVAL      vinflg = '2'
046800000801     C                   ELSE
046900010201     C                   ADD       1             §CTROKVB
047000000801     C                   ENDIF
047100000801     C*
047200000801     C                   ENDSR
047300010201     C*----------------------------------------------------*
047400021113     C*  CARICAMENTO CAMPI DA FLAT FILE (X FIVAD)
047500010201     C*----------------------------------------------------*
047600010201     C     CARVAD        BEGSR
047700010201     C*
047800010201     C* Carico la schiera dei codici prodotto per cliente/spedizione
047900010201     C                   if        %trim(%subst(vindta:319:8)) <> 'SFUSI'
048000010201     C                   add       1             i
048100010305     C                   eval      skcdpvad(i)=%subst(vindta:319:5)
048200010201     C                   else
048300010201     C                   add       1             i
048400010201     C                   eval      skcdpvad(i)=%subst(vindta:327:5)
048500010201     C                   add       1             i
048600010201     C                   eval      skcdpvad(i)=%subst(vindta:332:5)
048700010201     C                   add       1             i
048800010201     C                   eval      skcdpvad(i)=%subst(vindta:337:5)
048900010201     C                   endif
049000010201     C*
049100010201     C                   ENDSR
049200010201     C*----------------------------------------------------*
049300021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAD)
049400010201     C*----------------------------------------------------*
049500010305     C     WRIVAD        BEGSR
049600010201     C*
049700010201     C* Ordino la schera
049800010201     C                   sorta     skcdpvad
049900010201     C*
050000010201     C                   z-add     1             i
050100010201     C                   z-add     *zeros        cntcdp
050200010201     C* Mi posiziono sul primo elemento pieno della schiera e porto avanti l'indice
050300010202     C                   dow       skcdpvad(i) = *blanks and
050400021010     C                             i < 2000
050500010201     C                   add       1             i
050600010201     C                   enddo
050700010202     C*
050800021010     C                   if        i <= 2000
050900010201     C* Mi salvo il primo codice prodotto
051000010305     C                   eval      depcdp=%subst(skcdpvad(i):1:4)
051100010305     C* Inizio il ciclo principale
051200021010     C                   dow       i <= 2000
051300010305     C* E calcolo il n° di pezzi relativi a quel codice prodotto x determinare il n° colli del VAB
051400010305     C                   eval      PiStr=%subst(skcdpvad(i):5:1)
051500010305     C                   exsr      CHKNUM
051600010305     C                   if        PiInt=*on
051700010305     C                   z-add     PiVal         depcnt
051800010305     C                   else
051900010305     C                   z-add     1             depcnt
052000010305     C                   endif
052100010305     C                   eval      cntpez = cntpez + depcnt
052200010201     C* Effettuo considerazioni sulla totalizzazione del dettaglio colli per codice prodotto
052300010305     C                   if        depcdp = %subst(skcdpvad(i):1:4)
052400010305     C                   add       depcnt        cntcdp
052500010201     C                   else
052600021113     C                   clear                   FIVAD000
052700021113     C* Valorizzo l buffer di scrittura del FIVAD
052800010201     C                   eval      VADCCM = s_VABCCM
052900010201     C                   eval      VADLNP = s_VABLNP
053000010201     C                   eval      VADAAS = s_VABAAS
053001021113     C                   eval      VADFGS = s_VABFGS
053100010201     C                   eval      VADNRS = s_VABNRS
053200010201     C                   eval      VADNSP = s_VABNSP
053300010201     C                   eval      VADCDP = depcdp
053400010201     C                   eval      VADNCL = cntcdp
053500021113     C                   write     FIVAD000
053600010305     C                   eval      depcdp=%subst(skcdpvad(i):1:4)
053700010305     C                   z-add     depcnt        cntcdp
053800010201     C                   add       1             §CTROKVD
053900010201     C                   endif
054000010201     C                   add       1             i
054100010201     C                   enddo
054200010305     C*
054300010305     C* A questo punto so qunti sono i colli effettivi della testata della bolla
054400010305     C                   z-add     cntpez        VABNCL
054500010202     C*
054600010305     C* Quando esco dal ciclo scarico gli ultimi codici prodotto rimasti in "sospeso"
054700010202     C                   eval      VADCCM = s_VABCCM
054800010202     C                   eval      VADLNP = s_VABLNP
054900010202     C                   eval      VADAAS = s_VABAAS
054901021113     C                   eval      VADFGS = s_VABFGS
055000010202     C                   eval      VADNRS = s_VABNRS
055100010202     C                   eval      VADNSP = s_VABNSP
055200010202     C                   eval      VADCDP = depcdp
055300010202     C                   eval      VADNCL = cntcdp
055400021113     C                   write     FIVAD000
055500010202     C                   add       1             §CTROKVD
055600010202     C*
055700010202     C                   endif
055800010202     C*
055900010202     C* Re-inizializzo la schiera ed il suo indice
056000010202     C                   z-add     *zeros        i
056100010202     C                   clear                   skcdpvad
056200010201     C*
056300010201     C                   ENDSR
056400010202     C*----------------------------------------------------*
056500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR
056600010202     C*----------------------------------------------------*
056700010202     C     PREVAD        BEGSR
056800010202     C*
056900021113     C* Compongo il nome del membro da dare al FIVADWWR
057000010202     C                   eval      parmbr = vlrhdl
057100010202     C                   movel     'M'           parmbr
057200010203     C                   eval      parccm = '00491999'
057300010202     C                   eval      paropz = '1'
057400010202     C* Effettuo la chiamata al CLLE preposto
057500010202     C                   call(e)   'TIS7LAVC'
057600010202     C                   parm                    parccm
057700010202     C                   parm                    parmbr
057800010202     C                   parm                    paropz
057900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058000010202     C                   if        %error
058100010202     C                   movel     '1'           chkcall
058200010202     C                   else
058300010202     C                   movel     '0'           chkcall
058400010202     C                   endif
058500010202     C*
058600010202     C                   ENDSR
058700000801     C*----------------------------------------------------*
058800000801     C*  CONTROLLO NUMERICITA' CAMPI
058900000801     C*----------------------------------------------------*
059000000801     C     CHKNUM        BEGSR
059100000801     C*
059200000801     C                   call(e)   'ISNUMERIC'
059300000801     C                   PARM                    PiStr            30
059400000801     C                   PARM      ','           PiDecChr          1
059500000801     C                   PARM      *ZEROS        PiVal            30 9
059600000801     C                   PARM      '0'           PiInt             1
059700000801     C                   PARM      '0'           PiNum             1
059800000801     C                   IF        %error
059900000801     C                   EVAL      PiInt=*off
060000000801     C                   ENDIF
060100000801     C*
060200000801     C                   ENDSR
060300000801     C***
060400000801
060500011113
060600011113     C*----------------------------------------------------*
060700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060800011113     C*----------------------------------------------------*
060900011113     C     CHKIMPDIV     BEGSR
061000011113     C*
061100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061200011113     C                   Z-ADD     *zeros        wrkDec            9 9
061300011113     C*
061400011113     C* Come prima cosa effettuo considerazioni sulla divisa
061500011113     C                   IF        vabIAS > *zeros
061600011113     C                   IF        vabVAS <> 'EUR'
061700011113     C                   EVAL      vabVAS =  'ITL'
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C                   IF        vabCAS > *zeros
062200011113     C                   IF        vabVCA <> 'EUR'
062300011113     C                   EVAL      vabVCA =  'ITL'
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C                   IF        vabVMD > *zeros
062800011113     C                   IF        vabVAD <> 'EUR'
062900011113     C                   EVAL      vabVAD =  'ITL'
063000011113     C                   ENDIF
063100011113     C                   ENDIF
063200011113     C*
063300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063400011113     C                   Z-ADD     vabIAS        wrkDec
063500011113     C                   IF        wrkDec > *zeros
063600011113     C                   IF        vabVAS = 'ITL'
063700011113     C                   EVAL      vabIAS = *zeros
063800011113     C                   ENDIF
063900011113     C                   ENDIF
064000011113     C*
064100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064200011113     C                   Z-ADD     vabCAS        wrkDec
064300011113     C                   IF        wrkDec > *zeros
064400011113     C                   IF        vabVCA = 'ITL'
064500011113     C                   EVAL      vabCAS = *zeros
064600011113     C                   ENDIF
064700011113     C                   ENDIF
064800011113     C*
064900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065000011113     C                   Z-ADD     vabVMD        wrkDec
065100011113     C                   IF        wrkDec > *zeros
065200011113     C                   IF        vabVAD = 'ITL'
065300011113     C                   EVAL      vabVMD = *zeros
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C                   ENDSR
065800011113     C***
065900011113
066000011113
066100000801
066200000801
066300990920      /TITLE Invio dei dati al punto operativo.
066400010202     C     invio         BEGSR
066500990920     C*
066600021113     C* 1° invio FIVAD
066700010201     C                   reset                   dscmz
066800010201     C                   move      vlrpoi        cmzdst
066900021113     C                   eval      cmzfld = 'FIVADWWR'
067000010201     C                   eval      cmzmbd = vlrhdl
067100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067200021009     C***                if        prmfir = *blanks
067300021113     C                   eval      cmzfla = 'FIVAD00F'
067400021113     C                   eval      cmzmba = 'FIVAD00F'
067500021009     C***                else
067600021009     C***                eval      cmzfla = prmfir
067700021009     C***                eval      cmzmba = prmfir
067800021009     C***                endif
067900010201     C                   eval      cmznrr = *zeros
068000010201     C                   move      §ctrokvd      cmznrr
068001021111     C                   eval      cmzlba = vlrfl1
068100010201     C                   call(e)   'TIS711C'
068200010201     C                   parm                    dscmz
068300010201     C                   parm      *blanks       esito
068400010205     C                   if        %error
068500010205     C                             or cmzerr = '1'
068600010205     C                             or esito  = '1'
068700010205     C                   eval      wrkesito = '3'
068800010205     C                   else
068900010201     C*
069000021113     C* 2° invio FIVAB
069100010201     C                   reset                   dscmz
069200010201     C                   move      vlrpoi        cmzdst
069300010201     C                   eval      cmzfld = vlrfou
069400010201     C                   eval      cmzmbd = vlrhdl
069500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600021009     C***                if        prmfir = *blanks
069700021113     C                   eval      cmzfla = 'FIVAB00F'
069800021113     C                   eval      cmzmba = 'FIVAB00F'
069900021009     C***                else
070000021009     C***                eval      cmzfla = prmfir
070100021009     C***                eval      cmzmba = prmfir
070200021009     C***                endif
070300010201     C                   eval      cmznrr = *zeros
070400010201     C                   move      §ctrokvb      cmznrr
070401021111     C                   eval      cmzlba = vlrfl1
070500010201     C                   call(e)   'TIS711C'
070600010201     C                   parm                    dscmz
070700010201     C                   parm      *blanks       esito
070800010201     C                   if        %error
070900010201     C                             or cmzerr = '1'
071000010201     C                             or esito  = '1'
071100010201     C                   eval      wrkesito = '3'
071200010201     C                   endif
071300010205     C                   endif
071400990920     C*
071500000613     C                   ENDSR
071600000613     C***
071700990910
071800000613     C     *inzsr        BEGSR
071900990910     C*
072000990910     C     *entry        plist
072100990920     C                   parm                    tivlrds
072200990921     C                   parm      wrkesito      esito
072300000724     C                   parm                    prmlit
072400000710     C                   parm                    prmfir
072500000613     C*
072600000830     C* CALCOLA LA DATA CORRENTE
072700000830     C                   time                    wn14             14 0
072800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072900000830     C                   z-add     wn8           g08dat
073000000830     C                   z-add     *zeros        g08inv
073100000830     C                   movel     '0'           g08err
073200000830     C                   call      'XSRDA8'
073300000830     C                   parm                    wlbda8
073400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073500000830     C*
073600000613     C                   ENDSR
073700000613     C***
073800990908
073900010201     Otis7lava  E            riepilogo         2
074000990915     O                                              'Upload via Internet'
074100990915     O                                           +1 'Traduzione TIVIN00R -'
074200021113     O                                           +1 'FIVABWWR/FIVADWWR'
074300010201     O                                           +1 'Report testate bolle'
074400990915     O          E            riepilogo   2
074500990915     O                       wrkdata
074600990915     O                       wrkora              +1
074700990915     O                       procname            +1
074800990915     O          E            riepilogo   2
074900990915     O                                              'Cliente..................:'
075000990915     O                       VABCCM        z     +1
075100990915     O          E            riepilogo   2
075200990920     O                                              'Riferimento Strategi.....:'
075300990920     O                       vlrhdl              +1
075400990915     O          E            riepilogo   2
075500990915     O                                              'Giusti...................:'
075600010201     O                       §CTROKVB      2   +  1
075700990915     O          E            riepilogo   2
075800990915     O                                              'Sbagliati e corretti.....:'
075900971022     O                       §CTRMO        2   +  1
076000990915     O          E            riepilogo   2
076100990915     O                                              'Sbagliati e scartati.....:'
076200971022     O                       §CTRNO        2   +  1
076300000613
076400010201     Otis7lavas E            testdett          2
076500000613     O                                              'Upload via Internet'
076600000613     O                                           +1 'Traduzione TIVIN00R -'
076700021113     O                                           +1 'FIVABWWR/FIVADWWR'
076800010201     O                                           +1 'Report testate bolle'
076900000616     O          E            testdett    3
077000000613     O                                           +2 'N° rec'
077100000613     O                                           +3 'Anteprima contenuto'
077200000616     O          E            rigadett    2
077300000613     O                       rrnum               +2
077400000621     O                       recko               +3
077500000616     O          E            findett     2
077600000613     O                       wrkdata
077700000613     O                       wrkora              +1
077800000613     O                       procname            +1
077900000616     O          E            findett     2
078000000613     O                                              'Cliente..................:'
078100000613     O                       VABCCM        z     +1
078200000616     O          E            findett     2
078300000613     O                                              'Riferimento Strategi.....:'
078400000613     O                       vlrhdl              +1
078500000616     O          E            findett     2
078600000613     O                                              'Giusti...................:'
078700010201     O                       §CTROKVB      2   +  1
078800000616     O          E            findett     2
078900000613     O                                              'Sbagliati e corretti.....:'
079000000613     O                       §CTRMO        2   +  1
079100000616     O          E            findett     2
079200000613     O                                              'Sbagliati e scartati.....:'
079300000613     O                       §CTRNO        2   +  1
079400000512** CMD - COMANDI CL
079500011113OVRPRTF FILE(TIS7LAVA) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
079600011113OVRPRTF FILE(TIS7LAVAS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
079700010201DLTOVR FILE(TIS7LAVA TIS7LAVAS) LVL(*)
079800000512
079900000512
