000100070517      /TITLE Upload via Internet: traduzione FLBLCAAR in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600070517     FEDIVABwr  O    E             DISK    usropn
000700030416     Ftis7tlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030416     Ftis7tlps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000512     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100030416     D flblcaaf      e ds                  inz
002101070517     D tisi95ds      e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600990915     D wrkdata         s               d
002700990915     D wrkora          s               t
002800000613     D rrnum           s              6  0 INZ(*zeros)
002900000621     D recko           s            150    INZ(*blanks)
003000011123     D depcmd          s            150    INZ(*blanks)
003001070517     D wNomeFile       s             30    INZ(*blanks)
003002070517     D wCMR            s             35    INZ(*blanks)
003003070517     D wPosDaA         s              2    INZ(*blanks)
003004070517     D wPosDa          s              2  0 INZ(*zeros)
003005070517     D wLungA          s              2    INZ(*blanks)
003006070517     D wLung           s              2  0 INZ(*zeros)
003007070517     D wDataOraAlfa    s             14    INZ(*blanks)
003100010606     D*------------------
003200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300010606     D*------------------
003400010606     D WLBDA8          DS                  INZ
003500010606     D  G08DAT                 1      8  0
003600010606     D  G08INV                 9     16  0
003700010606     D  G08ERR                17     17
003800010606     D  G08TGI                18     22  0
003900010606     D*
004000990908
004100990915     C                   time                    wrkdata
004200990915     C                   time                    wrkora
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010530     C*
004700010530     C                   exsr      opeini
004800030416     C                   exsr      rwblc
004801070517     C*
004802070517     C* Esegue lancio TISI95R solo x chiusura
004803070517     C                   CLEAR                   TISI95DS
004804070517     C                   EVAL      I95TLA = 'C'
004805070517     C                   CALL      'TISI95R'
004806070517     C                   PARM                    TISI95DS
004900010530     C*
005000010530     C                   seton                                        lr
005100010530
005200010530
005300010530
005400010530
005500010530     C*--------------------------------------------------------
005600010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005700010530     C*--------------------------------------------------------
005800010530     C     PREELA        BEGSR
005900000724     C*
006000000724     C* SE OCCORRE SPEDIRE IN FILIALE
006100010530     C                   if        invfil <> *zeros and
006200010530     C                             flgGiro = '0'
006300010530     C*
006400010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006500010530     C                   eval      flgGiro = '1'
006600000724     C*
006700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006900010530     C     invfil        chain     azorg01l
007000000724     C                   if        %found
007100000616     C                   movel(p)  CMD(1)        depcmd
007200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007300000616     C*
007400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007500011123     C                   Z-ADD     150           LENGH            15 5
007600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000616     C                   PARM                    depcmd
007800000616     C                   PARM                    LENGH
007900000724     C*
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011123     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200030416     C                   if        not %open(tis7tlps)
009300030416     C                   open      tis7tlps
009400030407     C   50              except    testdett
009500000613     C                   endif
009600010530     C*
009700010530     C                   endif
009800010530     C*
009900010530     C                   ENDSR
010000010530     C***
010100010530
010200010530
010300010530
010400010530
010500010530
010600010530
010700010530     C*--------------------------------------------------------
010800010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010530     C*--------------------------------------------------------
011000010530     C     ENDELA        BEGSR
011100010530     C*
011200010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300000613     C*
011400030416     C                   if        %open(tis7tlps)
011500030407     C   50              except    findett
011600030416     C                   close     tis7tlps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011123     C                   Z-ADD     150           LENGH            15 5
012100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400010530     C*
012500010530     C                   ENDSR
012600010530     C***
012700000613
012800010530
012900010530
013000000613
013100000613     C*--------------------------------------------------------
013200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013300000613     C*--------------------------------------------------------
013400000621     C     STPR          BEGSR
013500000613     C*
013600030416     C                   if        not %open(tis7tlp)
013700030416     C                   open      tis7tlp
013800990915     C                   endif
013900990915     C*
014000030407     C   50              except    riepilogo
014100990915     C*
014200030416     C                   if        %open(tis7tlp)
014300030416     C                   close     tis7tlp
014400990914     C                   endif
014500990910     C*
014600000613     C                   ENDSR
014700000613     C***
014800030416
014900030416
015000030416
015100030416     C*----------------------------------------------------*
015200030416     C*  IMPOSTAZIONE CAMPI COSTANTI
015300030416     C*----------------------------------------------------*
015400030416     C     DEFCAM        BEGSR
015500030416     C*
015600030416     C* Verifico se sono stati passati come parametri
015700030416     C                   IF        vlrppt > *blanks
015800070517     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:7))
015900030416     C                   EXSR      CHKNUM
016000030416     C                   IF        PiInt=*on
016100030416     C                   Z-ADD     PiVal         VABCCM
016200030416     C                   ENDIF
016300070517     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:3))
016400030416     C                   EXSR      CHKNUM
016500030416     C                   IF        PiInt=*on
016600030416     C                   Z-ADD     PiVal         VABLNP
016700030416     C                   ENDIF
016800070517     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:3))
016900030416     C                   EXSR      CHKNUM
017000030416     C                   IF        PiInt=*on
017100030416     C                   Z-ADD     PiVal         VABCTR
017200030416     C                   ENDIF
017300030416     C                   ENDIF
017400030416     C*
017500030416     C                   ENDSR
017600030416     C*----------------------------------------------------*
017700010530
017800010530
017900010530
018000910830     C*--------------------------------------------------------
018100070517     C* RWBLC   LEGGE tivin00r E DA flblcaaf SCRIVE EDIVABWR *
018200910830     C*--------------------------------------------------------
018300030416     C     RWBLC         BEGSR
018301070517     C*
018302070517     C                   clear                   §CTROK
018303070517     C                   clear                   §CTRMO
018304070517     C                   clear                   §CTRNO
018401070517     C*
018402070517     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
018403070517     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
018404070517     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
018405070517     C                   if        *in60 = *on
018500990914     C                   if        not %open(tivin00r)
018600990908     C                   open      tivin00r
018700990914     C                   endif
018800070517     C                   if        not %open(edivabwr)
018900070517     C                   open      edivabwr
019000990914     C                   endif
019500990910     C*
019600921023     C                   DO        *HIVAL
019700990913     C*
019800990915     C                   READ      tivin00r                               70
019900010618     C*
020000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020100010618     C                   if        vinflg = '1'
020200010618     C                   eval      flgOk = '1'
020300010618     C                   endif
020400010705     C*
020500010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020600010705     C                   if        %subst(vindta:1:1) = '='
020700010705     C                   eval      %subst(vindta:1:1) = *blanks
020800010705     C                   endif
020900010705     C*
021000000905     C                   if        vindta > *blanks
021100000613     C                   add       1             rrnum
021200010601     C* ...elimino eventuali segni
021300010601     C     '+':' '       xlate     vindta        vindta
021400990913     C*
021500010601     C                   if        *in70 = *off and
021600990913     C                             (vinflg = *blanks
021700010711     C                              or vinflg = '0'
021800010711     C                              or vinflg = '2')
021900990913     C*
022000070517     C                   clear                   EDIVAB00
022100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022200010711     C                   if        vinflg = *blanks or vinflg = '0'
022300010711     C                   clear                   vinmsg
022400010711     C                   endif
022500010711     C*
022600030416     C                   eval      flblcaaf = vindta
022700030416     C*
022800971021     C                   SETOFF                                       303132
022900030416     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023000030416     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI OPPURE TUTTI BLANKS
023100030416     C                   TESTN                   ALCLNP               30
023200030416     C  N30              TESTN                   ALCLNP                 30
023300030416     C                   MOVE      ALCLNP        W001A             1
023400030416     C     *IN30         IFEQ      *OFF
023500030416     C     W001A         ORLT      '0'
023600030416     C                   SETON                                        31
023700030416     C                   EVAL      vinmsg = %trimr(vinmsg)
023800030416     C                             + ' ' + 'ALCLNP'
023900030416     C                   ELSE
024000030416     C                   MOVE      ALCLNP        VABLNP
024100030416     C     VABLNP        IFLE      *ZEROS
024200030416     C                   SETON                                        31
024300030416     C                   EVAL      vinmsg = %trimr(vinmsg)
024400030416     C                             + ' ' + 'ALCLNP'
024500030416     C                   ENDIF
024600030416     C                   ENDIF
024700030416     C*
024800030416     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024900030416     C                   eval      depfil = VABLNP
025000030416     C                   exsr      repfil
025100030416     C                   if        depfil = invfil
025200030416     C                   if        vlrpoi = 999
025300030416     C                   MOVE(P)   invfil        VABFGS
025400030416     C                   else
025500030416     C                   MOVE(P)   vlrpoi        VABFGS
025600030416     C                   endif
025700030416     C*
025800030416     C                   exsr      PREELA
025900030416     C*
026000030416     C                   TESTN                   ALCCCM               30
026100030416     C  N30              TESTN                   ALCCCM                 30
026200030416     C                   MOVE      ALCCCM        W001A             1
026300030416     C     *IN30         IFEQ      *OFF
026400030416     C     W001A         ORLT      '0'
026500030416     C                   SETON                                        31
026600030416     C                   EVAL      vinmsg = %trimr(vinmsg)
026700030416     C                             + ' ' + 'ALCCCM'
026800030416     C                   ELSE
026900030416     C                   MOVE      ALCCCM        VABCCM
027000030416     C     VABCCM        IFLE      *ZEROS
027100030416     C                   SETON                                        31
027200030416     C                   EVAL      vinmsg = %trimr(vinmsg)
027300030416     C                             + ' ' + 'ALCCCM'
027400030416     C                   ENDIF
027500030416     C                   ENDIF
027600040318     C*
027700040319     C                   MOVEL     datcor        VABAAS
027800030416     C*
027900030416     C                   TESTN                   ALCRMN               30
028000030416     C  N30              TESTN                   ALCRMN                 30
028100030416     C  N30              TESTN                   ALCRMN                   30
028200030416     C                   MOVE      ALCRMN        W001A             1
028300030416     C     *IN30         IFEQ      *OFF
028400030416     C     W001A         ORLT      '0'
028500030416     C                   MOVE      *ZERO         VABRMN
028600030416     C                   SETON                                        31
028700030416     C                   EVAL      vinmsg = %trimr(vinmsg)
028800030416     C                             + ' ' + 'ALCRMN'
028900030416     C                   ELSE
029000030416     C                   MOVE      ALCRMN        VABRMN
029100030416     C                   ENDIF
029200030416     C* SE ERRORI SCARTO
029300030416     C  N31              DO
029400030416     C                   MOVE      ALCATB        VABATB
029500030416     C                   MOVE      ALCMGS        VABMGS
029600030416     C                   MOVE      ALCNRS        VABNRS
029700030416     C                   MOVE      ALCNSP        VABNSP
029800030416     C                   MOVEL     ALCCBL        VABCBO
029900030416     C                   MOVE      ALCLNA        VABLNA
030000030416     C                   MOVEL     ALCRSD        VABRSD
030100030416     C                   MOVEL     ALCIND        VABIND
030200030416     C                   MOVE      ALCCAD        N5                5 0
030300030416     C                   MOVEL     N5            VABCAD
030400030416     C                   MOVEL     ALCLOD        VABLOD
030500030416     C                   MOVEL     ALCPRD        VABPRD
030600030416     C                   MOVEL     ALCGC1        VABGC1
030700030416     C                   MOVEL     ALCGC2        VABGC2
030800030416     C                   MOVE      ALCCTR        VABCTR
030900030416     C                   MOVE      ALCCTS        VABCTS
031000030416     C                   MOVE      ALCFTM        VABFTM
031100030416     C                   MOVE      ALCTSP        VABTSP
031200030416     C                   MOVE      ALCVAE        NUM9              9 0
031300030416     C                   Z-ADD     NUM9          VABIAS
031400030416     C                   MOVE      ALCNAS        VABNAS
031500030416     C                   MOVE      ALCNCL        VABNCL
031600030416     C* IMPOSTO LA VIRGOLA NEL PESO
031700030416     C                   MOVE      ALCPKG        §PKGD             1 0
031800030416     C                   MOVEL     ALCPKG        §PKGI             7 0
031900030416     C                   MOVE      §PKGI         §PKG6             6
032000030416     C                   MOVE      §PKGD         VABPKB
032100030416     C                   MOVEL     §PKG6         VABPKB
032101041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
032102041001     C                   IF        *IN51 = *OFF
032200030416     C* IMPOSTO LA VIRGOLA NEL VOLUME
032300030416     C                   MOVE      ALCVOL        §VOLD             2 0
032400030416     C                   MOVEL     ALCVOL        §VOLI             4 0
032500030416     C                   MOVE      §VOLI         §VOL3             3
032600030416     C                   MOVE      §VOLD         §VOL              5 2
032700030416     C                   MOVEL     §VOL3         §VOL
032800030416     C                   Z-ADD     §VOL          VABVLB
032801041001     C                   ELSE
032802041001     C* ...altrimenti forzo azzeramento del volume
032803041001     C                   Z-ADD     *zeros        VABVLB
032804041001     C                   ENDIF
032900030416     C*
033000030416     C                   MOVE      ALCANT        VABANT
033100030416     C                   MOVE      ALCASS        NUM9              9 0
033200030416     C                   Z-ADD     NUM9          VABCAS
033300030416     C                   MOVE      ALCTIC        VABTIC
033400030416     C                   MOVE      ALCCEI        VABTC1
033500030416     C                   MOVE      ALCSCL        VABSCL
033600030416     C                   MOVEL     ALCRAM        VABRMA
033700030416     C                   MOVEL     ALCRMO        VABRMO
033800030416     C                   MOVE      ALCNCD        VABNCD
033900030416     C                   MOVE      ALCNCA        VABNCA
034000030416     C                   MOVE      ALCXCO        VABXCO
034100030416     C                   MOVE      ALCNOT        VABNOT
034200030416     C                   MOVE      ALCZNC        VABZNC
034300030416     C                   MOVE      ALCCTM        VABCTM
034400030416     C                   MOVE      ALCFFD        VABFFD
034500030416     C                   MOVE      ALCDCR        VABDCR
034600030416     C                   MOVE      ALCTCR        VABTCR
034700030416     C                   MOVE      ALCHCR        VABHCR
034800030416     C                   ENDDO
034900030416     C*
035000030416     C* AL TERMINE IMPOSTO I VALORI PASSATI NEI PARAMETRI DEL TRADUTTORE
035100030416     C                   EXSR      DEFCAM
035101070517     C*
035102070517     C* VALORIZZO CAMPI RELATIVI AL "CMR"
035103070517     C                   EVAL      VABCMR = wCMR
035104070517     C                   EVAL      VABDCM = DATCOR
035105070517     C                   EVAL      VABDTS = DATCOR
035106070517     C                   EVAL      VABHMS = ORACOR
035107070517     C                   EVAL      VABCNT = 1
035108070517     C*
035109070517     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
035110070517     C                   IF        *IN52 = *ON
035111070517     C* Reperisco la provincia dal CAP e dalla località
035112070517     C                   IF        VABCAD <> *blanks AND
035113070517     C                             VABPRD  = *blanks AND
035114070517     C                             VABNZD  = *blanks
035115070517     C                   CLEAR                   TISI95DS
035116070517     C                   EVAL      I95TCN = '3'
035117070517     C                   Z-ADD     datcor        I95DAT
035118070517     C                   EVAL      I95CAP = VABCAD
035119070517     C                   EVAL      I95LOC = VABLOD
035120070517     C                   CALL      'TISI95R'
035121070517     C                   PARM                    TISI95DS
035122070517     C                   EVAL      VABPRD = O95PRV
035123070517     C                   ENDIF
035124070517     C                   ENDIF
035200930409     C*
035300010604     C  N31              ADD       1             §CTROK            7 0
035400010604     C   32              ADD       1             §CTRMO            7 0
035500010604     C   31              ADD       1             §CTRNO            7 0
035600070517     C  N31              WRITE     EDIVAB00
035700990910     C*
035800010601     C                   if        *in31 = *off and
035900010601     C                             *in32 = *off
036000990910     C                   eval      vinflg = '1'
036100990910     C                   else
036200000621     C                   eval      recko = vindta
036300030416     C                   if        %open(tis7tlps)
036400030407     C   50              except    rigadett
036500010612     C                   endif
036600990910     C                   eval      vinflg = '2'
036700990910     C                   endif
036800990910     C                   endif
036900010601     C*
037000010601     C                   endif
037100000905     C*
037200000905     C                   else
037300000905     C                   eval      vinflg = '1'
037400000905     C                   endif
037500000905     C*
037600000905     C  N70              update    tivin000
037700991022     C*
037800991022     C  N70              ENDdo
037900010530     C
038000010530     C*
038100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
038200010530     C                   if        cntNonEl = *zeros or
038300010530     C                             flgMulti = '0'
038400990910     C* Se non ci sono record con errori ...
038500010530     C                   if        §ctrno = 0 and
038600010601     C                             §ctrmo = 0 and
038700010530     C                             flgStato <> '2'
038800990910     C* ... restituisco esito OK.
038900990921     C                   eval      wrkesito = '0'
039000990910     C                   else
039100010601     C                   if        §ctrok > 0
039200990921     C                   eval      wrkesito = '1'
039300000710     C                   else
039400010615     C                   if        flgOk = '0'
039500010615     C                   eval      wrkesito = '2'
039600010615     C                   else
039700010615     C                   eval      wrkesito = '6'
039800010615     C                   endif
039900990910     C                   endif
040000000710     C                   endif
040100010530     C                   else
040200010530     C                   eval      wrkesito = '9'
040300010530     C                   endif
040301070517     C*
040302070517 X  1C                   else
040303070517     C                   eval      wrkesito = '8'
040304070517     C                   eval      flgGiro = '1'
040305070517 E  1C                   endif
040400990910     C*
040500990914     C                   if        %open(tivin00r)
040600990908     C                   close     tivin00r
040700990914     C                   endif
040800070517     C                   if        %open(edivabwr)
040900070517     C                   close     edivabwr
041000990914     C                   endif
041100010530     C*
041200010530     C                   if        vlrpoi <> 999
041300010530     C                   eval      invfil = vlrpoi
041400010530     C                   endif
041500990910     C*
041600990920     C                   if        §ctrok > 0
041700010530     C                             and invfil > *zeros
041800000613     C                   exsr      invio
041900990920     C                   endif
042000010612     C*
042100010612     C                   if        flgGiro = '1'
042200010612     C                   exsr      endela
042300010612     C                   endif
042400990920     C*
042500910830     C                   ENDSR
042600030416     C*----------------------------------------------------*
042700030416
042800030416
042900030416
043000030416
043100030416     C*----------------------------------------------------*
043200030416     C*  CONTROLLO NUMERICITA' CAMPI
043300030416     C*----------------------------------------------------*
043400030416     C     CHKNUM        BEGSR
043500030416     C*
043600030416     C                   call(e)   'ISNUMERIC'
043700030416     C                   PARM                    PiStr            30
043800030416     C                   PARM      ','           PiDecChr          1
043900030416     C                   PARM      *ZEROS        PiVal            30 9
044000030416     C                   PARM      '0'           PiInt             1
044100030416     C                   PARM      '0'           PiNum             1
044200030416     C                   IF        %error
044300030416     C                   EVAL      PiInt=*off
044400030416     C                   EVAL      PiNum=*off
044500030416     C                   ENDIF
044600030416     C*
044700030416     C                   ENDSR
044800030416     C*----------------------------------------------------*
044900990920
045000010530
045100010530
045200010530
045300010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045400010530     C     repfil        BEGSR
045500010530     C*
045600010530     C                   if        invfil = *zeros and
045700010530     C                             depfil > *zeros and
045800010613     C                             (vinflg = *blanks or
045900010613     C                              vinflg = *zeros)
046000010530     C
046100010530     C                   eval      invfil = depfil
046200010530     C                   endif
046300010530     C*
046400010530     C                   if        depfil <> invfil and
046500010530     C                             invfil > *zeros
046600010530     C                   eval      flgMulti = '1'
046700010530     C                   if        vinflg = *blanks
046800010530     C                   add       1             cntNonEl
046900010530     C                   endif
047000010530     C                   endif
047100010530     C*
047200010530     C                   if        vinflg = '2'
047300010530     C                   eval      flgStato = '2'
047400010530     C                   endif
047500010530     C*
047600010530     C                   ENDSR
047700010530     C***
047800010530
047900010530
048000010530
048100010530
048200990920      /TITLE Invio dei dati al punto operativo.
048300000613     C     invio         BEGSR
048400990920     C*
048500990920     C                   reset                   dscmz
048600010530     C                   move      invfil        cmzdst
048700990920     C                   eval      cmzfld = vlrfou
048800990920     C                   eval      cmzmbd = vlrhdl
048900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
049000000710     C                   if        prmfir = *blanks
049100070517     C                   eval      cmzfla = 'EDIVAB0F'
049200070517     C                   eval      cmzmba = 'EDIVAB0F'
049300000710     C                   else
049400000710     C                   eval      cmzfla = prmfir
049500000710     C                   eval      cmzmba = prmfir
049600000710     C                   endif
049700990920     C                   eval      cmznrr = *zeros
049800990920     C                   move      §ctrok        cmznrr
049900021003     C                   eval      cmzlba = vlrfl1
050000990920     C                   call(e)   'TIS711C'
050100990920     C                   parm                    dscmz
050200990921     C                   parm      *blanks       esito
050300990920     C                   if        %error
050400990920     C                             or cmzerr = '1'
050500990921     C                             or esito  = '1'
050600000710     C                   eval      wrkesito = '3'
050700990920     C                   endif
050800990920     C*
050900000613     C                   ENDSR
051000000613     C***
051100010530
051200010530
051300010530
051400010530
051500010530      /TITLE Invio dei dati al punto operativo.
051600010530     C     opeini        BEGSR
051700010530     C*
051800010530     C* Inizializzo flag e contatori operativi
051900010530     C                   movel     '0'           flgGiro           1
052000010530     C                   movel     '0'           flgMulti          1
052100010530     C                   movel     '1'           flgStato          1
052200010615     C                   movel     '0'           flgOk             1
052300010530     C                   z-add     *zeros        cntNonEl         10 0
052400010530     C                   z-add     *zeros        depfil            3 0
052500010530     C                   z-add     *zeros        invfil            3 0
053300010530     C*
053400010530     C                   ENDSR
053500010530     C***
053600010530
053700010530
053800010530
053900990910
054000000613     C     *inzsr        BEGSR
054100990910     C*
054200990910     C     *entry        plist
054300990920     C                   parm                    tivlrds
054400990921     C                   parm      wrkesito      esito
054500000724     C                   parm                    prmlit
054600000710     C                   parm                    prmfir
054601070517     C*
054602070517     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
054603070517     C                   move(p)   vlrMSG        wNomeFile
054604070517     C*
054605070517     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
054606070517     C* del codice CMR dal nome del file importato.
054607070517     C                   if        wNomeFile <> *blanks AND
054608070517     C                             vlrppt<>*blanks
054609070517     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
054610070517     C                   if        %subst(vlrppt:1:1) = 'S'
054611070517     C                   seton                                        50
054612070517     C                   else
054613070517     C                   setoff                                       50
054614070517     C                   endif
054615070517     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
054616070517     C                   if        %subst(vlrppt:6:1) = 'S'
054617070517     C                   seton                                        51
054618070517     C                   else
054619070517     C                   setoff                                       51
054620070517     C                   endif
054621070517     C*
054622070517     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
054623070517     C* (naturalmente solo se nn già presente)
054624070517     C                   if        %subst(vlrppt:7:1) = 'S'
054625070517     C                   seton                                        52
054626070517     C                   else
054627070517     C                   setoff                                       52
054628070517     C                   endif
054629070517     C*
054630070517     C                   eval      wPosDaA = %subst(vlrppt:2:2)
054631070517     C                   move(p)   wPosDaA       wPosDa
054632070517     C                   eval      wLungA  = %subst(vlrppt:4:2)
054633070517     C                   move(p)   wLungA        wLung
054634070517     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
054635070517     C                   seton                                        60
054636070517     C                   else
054637070517     C                   if        vlrisv = 'UL'
054638070517     C                   movel     datcor        wDataOraAlfa
054639070517     C                   move      oracor        wDataOraAlfa
054640070517     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
054641070517     C                   seton                                        60
054642070517     C                   else
054643070517     C                   setoff                                       60
054644070517     C                   endif
054645070517     C                   endif
054700010606     C*
054800010606     C* CALCOLA LA DATA CORRENTE
054900010606     C                   time                    wn14             14 0
055000010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055001070517     C                   movel     wn14          oracor            6 0          *ORA
055100010606     C                   z-add     wn8           g08dat
055200010606     C                   z-add     *zeros        g08inv
055300010606     C                   movel     '0'           g08err
055400010606     C                   call      'XSRDA8'
055500010606     C                   parm                    wlbda8
055600010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055700000613     C*
055800000613     C                   ENDSR
055900000613     C***
056000990908
056100030416     Otis7tlp   E            riepilogo         2
056200990915     O                                              'Upload via Internet'
056300990915     O                                           +1 'Traduzione TIVIN00R -'
056400070517     O                                           +1 'EDIVABWR'
056500990915     O                                           +1 'Testate bolle'
056600990915     O          E            riepilogo   2
056700990915     O                       wrkdata
056800990915     O                       wrkora              +1
056900990915     O                       procname            +1
057000990915     O          E            riepilogo   2
057100990915     O                                              'Cliente..................:'
057200990915     O                       VABCCM        z     +1
057300990915     O          E            riepilogo   2
057400990920     O                                              'Riferimento Strategi.....:'
057500990920     O                       vlrhdl              +1
057600990915     O          E            riepilogo   2
057700990915     O                                              'Giusti...................:'
057800971022     O                       §CTROK        2   +  1
057900990915     O          E            riepilogo   2
058000990915     O                                              'Sbagliati e corretti.....:'
058100971022     O                       §CTRMO        2   +  1
058200990915     O          E            riepilogo   2
058300990915     O                                              'Sbagliati e scartati.....:'
058400971022     O                       §CTRNO        2   +  1
058500000613
058600030416     Otis7tlps  E            testdett          2
058700000613     O                                              'Upload via Internet'
058800000613     O                                           +1 'Traduzione TIVIN00R -'
058900070517     O                                           +1 'EDIVABWR'
059000000613     O                                           +1 'Testate bolle'
059100000616     O          E            testdett    3
059200000613     O                                           +2 'N° rec'
059300000613     O                                           +3 'Anteprima contenuto'
059400000616     O          E            rigadett    2
059500000613     O                       rrnum               +2
059600000621     O                       recko               +3
059700000616     O          E            findett     2
059800000613     O                       wrkdata
059900000613     O                       wrkora              +1
060000000613     O                       procname            +1
060100000616     O          E            findett     2
060200000613     O                                              'Cliente..................:'
060300000613     O                       VABCCM        z     +1
060400000616     O          E            findett     2
060500000613     O                                              'Riferimento Strategi.....:'
060600000613     O                       vlrhdl              +1
060700000616     O          E            findett     2
060800000613     O                                              'Giusti...................:'
060900000613     O                       §CTROK        2   +  1
061000000616     O          E            findett     2
061100000613     O                                              'Sbagliati e corretti.....:'
061200000613     O                       §CTRMO        2   +  1
061300000616     O          E            findett     2
061400000613     O                                              'Sbagliati e scartati.....:'
061500000613     O                       §CTRNO        2   +  1
061600000512** CMD - COMANDI CL
061700030416OVRPRTF FILE(TIS7TLP) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
061800030416OVRPRTF FILE(TIS7TLPS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
061900030416DLTOVR FILE(TIS7TLP TIS7TLPS) LVL(*)
062000000512
062100000512
