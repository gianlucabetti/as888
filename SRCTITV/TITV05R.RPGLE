000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800020305     Ftitv05p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100020305     Ftitv05ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500020305     D depspe          s             14    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700010213     D depaas          s              4    INZ(*zeros)
003800020305     D curspe          s             14    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300000801
004400010213     D                 DS                  INZ
004500010213     D  aaaa                   1      4  0
004600010213     D  mmgg                   5      8  0
004700010213     D  mm                     5      6  0
004800010213     D  gg                     7      8  0
004900010213     D  datawrk                1      8  0
005000010213
005100010213     D  Num8_0         s              8  0
005200000801     D  Num5_0         s              5  0
005300000830
005400000830     D*------------------
005500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600000830     D*------------------
005700000830     D WLBDA8          DS                  INZ
005800000830     D  G08DAT                 1      8  0
005900000830     D  G08INV                 9     16  0
006000000830     D  G08ERR                17     17
006100000830     D  G08TGI                18     22  0
006200000830     D*
006300990908
006400010201
006500010201
006600990915     C                   time                    wrkdata
006700990915     C                   time                    wrkora
006800000913     C                   reset                   rrnum
006900990921     C                   reset                   esito
007000990921     C                   reset                   wrkesito
007100000724     C*
007200000724     C* SE OCCORRE SPEDIRE IN FILIALE
007300000724     C                   if        vlrpoi <> *zeros
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700000724     C     vlrpoi        chain     azorg01l
007800000724     C                   if        %found
007900000616     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000724     C                   endif
009000000616     C*
009100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000616     C                   movel(p)  CMD(2)        depcmd
009300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009400000616     C*
009500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009600011113     C                   Z-ADD     150           LENGH            15 5
009700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800000616     C                   PARM                    depcmd
009900000616     C                   PARM                    LENGH
010000000616     C*
010100020305     C                   if        not %open(titv05ps)
010200020305     C                   open      titv05ps
010300000616     C                   except    testdett
010400000613     C                   endif
010500000613     C*
010600020305     C                   EXSR      RWPRADA                                      LETT/SCR. VAB
010700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010800000613     C*
010900010202     C* Effettuo la chiamata al CLLE preposto
011000050128     C                   call(e)   'TITVVTC'
011100010202     C                   parm                    parccm
011200010202     C                   parm                    parmbr
011300010202     C                   parm      '2'           paropz
011400010202     C*
011500020305     C                   if        %open(titv05ps)
011600000616     C                   except    findett
011700020305     C                   close     titv05ps
011800000613     C                   endif
011900000616     C*
012000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    CMD(3)
012400000616     C                   PARM                    LENGH
012500000616     C*
012600000801     C
012700010201     C                   seton                                        LR
012800000613
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500020305     C                   if        not %open(titv05p)
013600020305     C                   open      titv05p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100020305     C                   if        %open(titv05p)
014200020305     C                   close     titv05p
014300990914     C                   endif
014400990910     C*
014500000613     C                   ENDSR
014600000613     C***
014700990908
014800000801
014900910830     C*--------------------------------------------------------
015000021113     C* RWPRADA  LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015100910830     C*--------------------------------------------------------
015200020305     C     RWPRADA       BEGSR
015300990910     C*
015400990914     C                   if        not %open(tivin00r)
015500990908     C                   open      tivin00r
015600990914     C                   endif
015700021113     C                   if        not %open(fivabwwr)
015800021113     C                   open      fivabwwr
015900990914     C                   endif
016000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016100020305     C                   exsr      prevat
016200010201     C*
016300010202     C                   if        chkcall = '0'
016400010202     C*
016500021113     C                   if        not %open(fivatwwr)
016600021113     C                   open      fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   clear                   §CTROKVB          5 0
017000020305     C                   clear                   §CTROKVT          5 0
017100000801     C                   clear                   §CTRMO            5 0
017200000801     C                   clear                   §CTRNO            5 0
017300990910     C*
017400921023     C                   DO        *HIVAL
017500990913     C*
017600990915     C                   READ      tivin00r                               70
017700000905     C                   if        vindta > *blanks
017800000613     C                   add       1             rrnum
017900000801     C*
018000000801     C                   if        *in70 = *off
018100000801     C                             and
018200000801     C                             (vinflg = *blanks
018300000801     C                              or vinflg = '0'
018400000801     C                              or vinflg = '2')
018500000801     C*
018600000801     C                   clear                   vinmsg
018700000801     C                   eval      vinflg = '1'
018800010305     C*
018900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019000061027     C                   EVAL      PiStr=%trim(%subst(vindta:122:14))
019100020305     C                   MOVEL(p)  PiStr         curspe
019200010305     C*
019300010305     C                   if        depspe = *blanks                             => 1° giro
019400010305     C                   eval      depspe = curspe                              => memorizz. spediz
019500020305     C                   exsr      impvab
019600050202     C  N60              exsr      wrivat                                       => carico VAT
019700010305     C                   else
019800010305     C                   if        curspe <> depspe                             => rottura di spediz
019900010305     C                   eval      depspe = curspe                              => memorizz. spediz
020000050202     C  N60              exsr      wrivab
020100020305     C                   exsr      impvab
020200050202     C  N60              exsr      wrivat                                       => carico VAT
020300020305     C                   else                                                   => x stessa spediz
020400020305     C                   exsr      impvab
020500050202     C  N60              exsr      wrivat                                       => carico VAT
020600010305     C                   endif
020700010305     C                   endif
020800010305     C                   endif
020900000905     C*
021000000905     C                   else
021100000905     C                   eval      vinflg = '1'
021200000905     C                   endif
021300000905     C*
021400000905     C  N70              update    tivin000
021500000905     C*
021600991022     C  N70              ENDdo
021700010202     C*
021800020305     C* Scarico i VAB rimasti "in sospeso"
021900050202     C  N60              exsr      wrivab
022000010202     C*
022100010202     C                   endif
022200990910
022300990910     C* Se non ci sono record con errori ...
022400000710     C                   if        §ctrno = 0
022500990910     C* ... restituisco esito OK.
022600990921     C                   eval      wrkesito = '0'
022700990910     C                   else
022800010201     C                   if        §ctrokvb > 0
022900990921     C                   eval      wrkesito = '1'
023000000710     C                   else
023100000710     C                   eval      wrkesito = '2'
023200990910     C                   endif
023300000710     C                   endif
023400990910     C*
023500990914     C                   if        %open(tivin00r)
023600990908     C                   close     tivin00r
023700990914     C                   endif
023800021113     C                   if        %open(fivabwwr)
023900021113     C                   close     fivabwwr
024000990914     C                   endif
024100021113     C                   if        %open(fivatwwr)
024200021113     C                   close     fivatwwr
024300010201     C                   endif
024400990910     C*
024500010201     C                   if        §ctrokvb > 0
024600000724     C                             and vlrpoi <> *zeros
024700010202     C                   exsr      invio
024800990920     C                   endif
024900990920     C*
025000910830     C                   ENDSR
025100000613     C***
025200010305
025300010305     C*----------------------------------------------------*
025400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025500010305     C*----------------------------------------------------*
025600020305     C     WRIVAB        BEGSR
025700010305     C*
025800021113     C                   write     fivab000                                     => scarico il VAB
025900010305     C*
026000010305     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700010201     C                   Z-ADD     *zeros        Num5_0
026800000801     C*
026900000801     C                   ENDSR
027000000801     C*----------------------------------------------------*
027100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027200000801     C*----------------------------------------------------*
027300000801     C     DEFCAM        BEGSR
027400000801     C*
027500021113     C                   CLEAR                   FIVAB000
027600020305     C                   Z-ADD     0282998       VABCCM
027700020305     C                   Z-ADD     0282998       VATCCM
027800020305     C                   Z-ADD     028           VABLNP
027900020305     C                   Z-ADD     028           VATLNP
028000010201     C                   Z-ADD     000           VABCTR
028100020305     C                   MOVEL     '7Q'          VABCTM
028200020305     C                   MOVEL     'E'           VATTRC
028300000801     C*
028400000801     C                   ENDSR
028500000801     C*----------------------------------------------------*
028600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028700000801     C*----------------------------------------------------*
028800010201     C     IMPVAB        BEGSR
028900000801     C*
029000020305     C                   EXSR      INZVAR
029100020305     C                   EXSR      DEFCAM
029200010305     C*
029300050202     C                   SETOFF                                       60
029400000801     C                   Z-ADD     *zeros        errore            1 0
029500000830     C                   MOVEL     datcor        VABAAS
029600020305     C                   MOVEL     datcor        VATAAS
029700021113     C                   MOVE(P)   vlrpoi        VABFGS
029800021113     C                   MOVE(P)   vlrpoi        VATFGS
029900010213     C                   MOVEL     VABAAS        depaas
030000000830     C                   MOVE      datcor        VABMGS
030100050511     C                   EVAL      VABRSD=%trim(%subst(vindta:225:30))
030200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030400020117     C* ==
030500061027     C                   EVAL      VABIND=%trim(%subst(vindta:255:30))
030600061027     C                   EVAL      VABRMA=%trim(%subst(vindta:122:14))
030700061027     C                   EVAL      VABLOD=%trim(%subst(vindta:291:30))
030800061027     C                   EVAL      VABPRD=%trim(%subst(vindta:321:2))
030900061027     C                   EVAL      VABNOT=%trim(%subst(vindta:335:50))
031000061027     C                   EVAL      VABNT2=%trim(%subst(vindta:417:14))+'-'+
031100061027     C                                    %trim(%subst(vindta:385:20))
031200020305     C*
031300061027     C                   EVAL      PiStr=%trim(%subst(vindta:286:5))
031400000801     C                   EXSR      CHKNUM
031500000801     C                   IF        PiInt=*on
031600000801     C                   Z-ADD     PiVal         Num5_0
031700010208     C                   MOVEL(p)  Num5_0        VABCAD
031800000801     C                   ELSE
031900000801     C                   ADD       1             errore
032000000801     C                   EVAL      vinmsg = %trimr(vinmsg)
032100000801     C                             + ' ' + 'VABCAD'
032200000801     C                   ENDIF
032300020305     C*
032400061027     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
032500010201     C                   EXSR      CHKNUM
032600010201     C                   IF        PiInt=*on
032700010201     C                   Z-ADD     PiVal         Num5_0
032800010208     C                   MOVE      Num5_0        VABNCL
032900010201     C                   ELSE
033000010201     C                   ADD       1             errore
033100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033200010201     C                             + ' ' + 'VABNCL'
033300010201     C                   ENDIF
033400020305     C*
033500061027     C                   EVAL      PiStr=%trim(%subst(vindta:191:10))
033600010201     C                   EXSR      CHKNUM
033700010201     C                   IF        PiNum=*on
033800011115     C                   Z-ADD     PiVal         VABPKB
033900010201     C                   ELSE
034000010201     C                   ADD       1             errore
034100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034200010201     C                             + ' ' + 'VABPKB'
034300010201     C                   ENDIF
034400020305     C*
034500061027     C                   EVAL      PiStr=%trim(%subst(vindta:202:14))
034600010202     C                   IF        PiStr>*blanks
034700010201     C                   EXSR      CHKNUM
034800010201     C                   IF        PiNum=*on
034900020305     C                   Z-ADD(H)  PiVal         VABVLB
035000010201     C                   ELSE
035100010201     C                   ADD       1             errore
035200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035300010201     C                             + ' ' + 'VABVLB'
035400010201     C                   ENDIF
035500010202     C                   ENDIF
035600020305     C*
035700061027     C                   EVAL      PiStr=%trim(%subst(vindta:122+7:7))
035800010201     C                   EXSR      CHKNUM
035900010201     C                   IF        PiInt=*on
036000050511     C                   Z-ADD     PiVal         VABRMN
036100011115     C                   Z-ADD     PiVal         VABNSP
036200020305     C                   Z-ADD     PiVal         VATNSP
036300010201     C                   ELSE
036400050202     C                   SETON                                        60
036500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036600050511     C                             + ' ' + 'VABNSP VATNSP VABRMN'
036700010201     C                   ENDIF
036800010205     C*
036900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037000010205     C                   IF        VABCAS > *zeros
037100010205     C                   EVAL      VABCBO = '4'
037200010205     C                   ELSE
037300010205     C                   EVAL      VABCBO = '1'
037400010205     C                   ENDIF
037500020305     C*
037600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037700011113     C                   EXSR      CHKIMPDIV
037800010202     C*
037900000801     C* Ebbene...
038000050202     C                   IF        *IN60 = *ON
038100000801     C                   ADD       1             §CTRNO
038200000801     C                   EVAL      recko = vindta
038300000801     C                   EXCEPT    rigadett
038400000801     C                   EVAL      vinflg = '2'
038500000801     C                   ELSE
038600050202     C                   ADD       1             §CTROKVB
038700050202     C                   IF        errore <> *zeros
038800050202     C                   ADD       1             §CTRMO
038900050202     C                   ENDIF
039000000801     C                   ENDIF
039100000801     C*
039200000801     C                   ENDSR
039300010201     C*----------------------------------------------------*
039400021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
039500010201     C*----------------------------------------------------*
039600020305     C     WRIVAT        BEGSR
039700010201     C*
039800021113     C* Valorizzo l buffer di scrittura del FIVAT
039900061027     C                   eval      VATNOT = %subst(vindta:432:20)
040000021113     C                   write     FIVAT000
040100010201     C*
040200010201     C                   ENDSR
040300010202     C*----------------------------------------------------*
040400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040500010202     C*----------------------------------------------------*
040600020305     C     PREVAT        BEGSR
040700010202     C*
040800021113     C* Compongo il nome del membro da dare al FIVATWWR
040900010202     C                   eval      parmbr = vlrhdl
041000010202     C                   movel     'M'           parmbr
041100020305     C                   eval      parccm = '00282998'
041200010202     C                   eval      paropz = '1'
041300010202     C* Effettuo la chiamata al CLLE preposto
041400050128     C                   call(e)   'TITVVTC'
041500010202     C                   parm                    parccm
041600010202     C                   parm                    parmbr
041700010202     C                   parm                    paropz
041800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041900010202     C                   if        %error
042000010202     C                   movel     '1'           chkcall
042100010202     C                   else
042200010202     C                   movel     '0'           chkcall
042300010202     C                   endif
042400010202     C*
042500010202     C                   ENDSR
042600000801     C*----------------------------------------------------*
042700000801     C*  CONTROLLO NUMERICITA' CAMPI
042800000801     C*----------------------------------------------------*
042900000801     C     CHKNUM        BEGSR
043000000801     C*
043100000801     C                   call(e)   'ISNUMERIC'
043200000801     C                   PARM                    PiStr            30
043300000801     C                   PARM      ','           PiDecChr          1
043400000801     C                   PARM      *ZEROS        PiVal            30 9
043500000801     C                   PARM      '0'           PiInt             1
043600000801     C                   PARM      '0'           PiNum             1
043700000801     C                   IF        %error
043800000801     C                   EVAL      PiInt=*off
043900000801     C                   ENDIF
044000000801     C*
044100000801     C                   ENDSR
044200000801     C***
044300000801
044400011113
044500011113     C*----------------------------------------------------*
044600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
044700011113     C*----------------------------------------------------*
044800011113     C     CHKIMPDIV     BEGSR
044900011113     C*
045000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045100011113     C                   Z-ADD     *zeros        wrkDec            9 9
045200011113     C*
045300011113     C* Come prima cosa effettuo considerazioni sulla divisa
045400011113     C                   IF        vabIAS > *zeros
045500011113     C                   IF        vabVAS <> 'EUR'
045600011113     C                   EVAL      vabVAS =  'ITL'
045700011113     C                   ENDIF
045800011113     C                   ENDIF
045900011113     C*
046000011113     C                   IF        vabCAS > *zeros
046100011113     C                   IF        vabVCA <> 'EUR'
046200011113     C                   EVAL      vabVCA =  'ITL'
046300011113     C                   ENDIF
046400011113     C                   ENDIF
046500011113     C*
046600011113     C                   IF        vabVMD > *zeros
046700020305     C                   IF        vabVAD <> 'EUR'
046800011113     C                   EVAL      vabVAD =  'ITL'
046900011113     C                   ENDIF
047000011113     C                   ENDIF
047100011113     C*
047200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
047300011113     C                   Z-ADD     vabIAS        wrkDec
047400011113     C                   IF        wrkDec > *zeros
047500011113     C                   IF        vabVAS = 'ITL'
047600011113     C                   EVAL      vabIAS = *zeros
047700011113     C                   ENDIF
047800011113     C                   ENDIF
047900011113     C*
048000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
048100011113     C                   Z-ADD     vabCAS        wrkDec
048200011113     C                   IF        wrkDec > *zeros
048300011113     C                   IF        vabVCA = 'ITL'
048400011113     C                   EVAL      vabCAS = *zeros
048500011113     C                   ENDIF
048600011113     C                   ENDIF
048700011113     C*
048800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
048900011113     C                   Z-ADD     vabVMD        wrkDec
049000011113     C                   IF        wrkDec > *zeros
049100011113     C                   IF        vabVAD = 'ITL'
049200011113     C                   EVAL      vabVMD = *zeros
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C                   ENDSR
049700011113     C***
049800011113
049900011113
050000000801
050100000801
050200990920      /TITLE Invio dei dati al punto operativo.
050300010202     C     invio         BEGSR
050400990920     C*
050500021113     C* 1° invio FIVAT
050600010201     C                   reset                   dscmz
050700010201     C                   move      vlrpoi        cmzdst
050800021113     C                   eval      cmzfld = 'FIVATWWR'
050900010201     C                   eval      cmzmbd = vlrhdl
051000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
051100021009     C***                if        prmfir = *blanks
051200021113     C                   eval      cmzfla = 'FIVAT00F'
051300021113     C                   eval      cmzmba = 'FIVAT00F'
051400021009     C***                else
051500021009     C***                eval      cmzfla = prmfir
051600021009     C***                eval      cmzmba = prmfir
051700021009     C***                endif
051800010201     C                   eval      cmznrr = *zeros
051900020305     C                   move      §ctrokvt      cmznrr
052000021018     C                   eval      cmzlba = vlrfl1
052100010201     C                   call(e)   'TIS711C'
052200010201     C                   parm                    dscmz
052300010201     C                   parm      *blanks       esito
052400010205     C                   if        %error
052500010205     C                             or cmzerr = '1'
052600010205     C                             or esito  = '1'
052700010205     C                   eval      wrkesito = '3'
052800010205     C                   else
052900010201     C*
053000021113     C* 2° invio FIVAB
053100010201     C                   reset                   dscmz
053200010201     C                   move      vlrpoi        cmzdst
053300010201     C                   eval      cmzfld = vlrfou
053400010201     C                   eval      cmzmbd = vlrhdl
053500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053600021009     C***                if        prmfir = *blanks
053700021113     C                   eval      cmzfla = 'FIVAB00F'
053800021113     C                   eval      cmzmba = 'FIVAB00F'
053900021009     C***                else
054000021009     C***                eval      cmzfla = prmfir
054100021009     C***                eval      cmzmba = prmfir
054200021009     C***                endif
054300010201     C                   eval      cmznrr = *zeros
054400010201     C                   move      §ctrokvb      cmznrr
054500021018     C                   eval      cmzlba = vlrfl1
054600010201     C                   call(e)   'TIS711C'
054700010201     C                   parm                    dscmz
054800010201     C                   parm      *blanks       esito
054900010201     C                   if        %error
055000010201     C                             or cmzerr = '1'
055100010201     C                             or esito  = '1'
055200010201     C                   eval      wrkesito = '3'
055300010201     C                   endif
055400010205     C                   endif
055500990920     C*
055600000613     C                   ENDSR
055700000613     C***
055800070411
055900070411     C     *pssr         BEGSR
056000070411     C*
056100070411     C                   if        %open(tivin00r)
056200070411     C                   close     tivin00r
056300070411     C                   endif
056400070411     C                   if        %open(fivabwwr)
056500070411     C                   close     fivabwwr
056600070411     C                   endif
056700070411     C                   if        %open(fivatwwr)
056800070411     C                   close     fivatwwr
056900070411     C                   endif
057000070411     C*
057100070411     C* Effettuo la chiamata al CLLE preposto
057200070411     C                   call(e)   'TITVVTC'
057300070411     C                   parm                    parccm
057400070411     C                   parm                    parmbr
057500070411     C                   parm      '2'           paropz
057600070411     C*
057700070411     C                   eval      wrkesito = '2'
057800070411     C*
057900070411     C                   seton                                        LR
058000070411     C*
058100070411     C                   ENDSR     '*CANCL'
058200070411     C***
058300070411
058400990910
058500000613     C     *inzsr        BEGSR
058600990910     C*
058700990910     C     *entry        plist
058800990920     C                   parm                    tivlrds
058900990921     C                   parm      wrkesito      esito
059000000724     C                   parm                    prmlit
059100000710     C                   parm                    prmfir
059200000613     C*
059300000830     C* CALCOLA LA DATA CORRENTE
059400000830     C                   time                    wn14             14 0
059500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059600000830     C                   z-add     wn8           g08dat
059700000830     C                   z-add     *zeros        g08inv
059800000830     C                   movel     '0'           g08err
059900000830     C                   call      'XSRDA8'
060000000830     C                   parm                    wlbda8
060100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
060200000830     C*
060300000613     C                   ENDSR
060400000613     C***
060500990908
060600020305     Otitv05p   E            riepilogo         2
060700990915     O                                              'Upload via Internet'
060800990915     O                                           +1 'Traduzione TIVIN00R -'
060900021113     O                                           +1 'FIVABWWR/FIVATWWR'
061000010201     O                                           +1 'Report testate bolle'
061100990915     O          E            riepilogo   2
061200990915     O                       wrkdata
061300990915     O                       wrkora              +1
061400990915     O                       procname            +1
061500990915     O          E            riepilogo   2
061600990915     O                                              'Cliente..................:'
061700990915     O                       VABCCM        z     +1
061800990915     O          E            riepilogo   2
061900990920     O                                              'Riferimento Strategi.....:'
062000990920     O                       vlrhdl              +1
062100990915     O          E            riepilogo   2
062200990915     O                                              'Giusti...................:'
062300010201     O                       §CTROKVB      2   +  1
062400990915     O          E            riepilogo   2
062500990915     O                                              'Sbagliati e corretti.....:'
062600971022     O                       §CTRMO        2   +  1
062700990915     O          E            riepilogo   2
062800990915     O                                              'Sbagliati e scartati.....:'
062900971022     O                       §CTRNO        2   +  1
063000000613
063100020305     Otitv05ps  E            testdett          2
063200000613     O                                              'Upload via Internet'
063300000613     O                                           +1 'Traduzione TIVIN00R -'
063400021113     O                                           +1 'FIVABWWR/FIVATWWR'
063500010201     O                                           +1 'Report testate bolle'
063600000616     O          E            testdett    3
063700000613     O                                           +2 'N° rec'
063800000613     O                                           +3 'Anteprima contenuto'
063900000616     O          E            rigadett    2
064000000613     O                       rrnum               +2
064100000621     O                       recko               +3
064200000616     O          E            findett     2
064300000613     O                       wrkdata
064400000613     O                       wrkora              +1
064500000613     O                       procname            +1
064600000616     O          E            findett     2
064700000613     O                                              'Cliente..................:'
064800000613     O                       VABCCM        z     +1
064900000616     O          E            findett     2
065000000613     O                                              'Riferimento Strategi.....:'
065100000613     O                       vlrhdl              +1
065200000616     O          E            findett     2
065300000613     O                                              'Giusti...................:'
065400010201     O                       §CTROKVB      2   +  1
065500000616     O          E            findett     2
065600000613     O                                              'Sbagliati e corretti.....:'
065700000613     O                       §CTRMO        2   +  1
065800000616     O          E            findett     2
065900000613     O                                              'Sbagliati e scartati.....:'
066000000613     O                       §CTRNO        2   +  1
066100000512** CMD - COMANDI CL
066200020305OVRPRTF FILE(TITV05P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
066300020305OVRPRTF FILE(TITV05PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
066400020305DLTOVR FILE(TITV05P TITV05PS) LVL(*)
066500000512
066600000512
