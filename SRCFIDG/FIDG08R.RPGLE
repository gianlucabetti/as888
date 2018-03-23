000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200070115     H* FIDG08R *-----------------------------------------------------*
000300070112     H*            riceraca giri autotrasportatore
000400930225     H*---------------------------------------------------------------*
000500070115     FFidg08D   CF   E             WORKSTN
000600070117     F                                     SFILE(LRF0S03:nrr)
000700021203     Ffiapd01L  IF   E           K DISK
000800061222     Ffiapd31L  IF   E           K DISK
000900070115     Ffiapd32L  IF   E           K DISK    rename(fiapd300:fiapd332)
000901070720     Ffiapd51L  IF   E           K DISK
000902070720     Ffiapd52L  IF   E           K DISK    rename(fiapd500:fiapd552)
001000070201     D VDESS           S                   LIKE(V1cdes)
001100030724     d dtaiso          s               d
001200030724     d dtaeur          s               d   datfmt(*eur)
001300070115     D fidg08ds      E DS
001400070129     D fidg09ds      E DS
001500900514     D KPJBA         E DS
001600061002     d* Parametri x Controllo profilo utenti
001700061002     d TIBS34ds      e ds
001800061002     d AZUTEds       e ds                  extname(AZUTE00F)
001900061002     d* Ds per dati organigramma
002000061002     d DDatiUte      e ds
002100070116     d*---
002200070116     d NotUsed         S             16A
002300070116     d ReturnSize      S              9B 0
002400070116     d SizeList        S              9B 0
002500070116     d WrkSort         S              1
002600070116     d*---
002700070117     d nrr             S              4  0                                      *n° rel sf
002800070117     d stmrec1         S              4  0                                      *n° rel sf x stampa
002900070116     d*---
003000070116     d MaxKey          C                   2
003100070116     d Ascendente      C                   1
003200070116     d Discendente     C                   2
003300070116     d Carattere       C                   6
003400070116     d Put             C                   1
003500070116     d EndPut          C                   2
003600070116     d Get             C                   3
003700070116     d Numerico        C                   8
003800070116     d*---
003900070116     d SflRcd          DS
004000070116     d  vscsce
004100070116     d  VSCatb
004200070116     d  VSCFPP
004300070129     d  VSDPDR
004400070129     d  VSDcgi
004500070116     d  VSCPDR
004600070116     d  VSCDDE
004700070119     d  VSCscad
004800070129     d  VSCfgsg
004900070129     d  VSCcgi
005000070119     d  VSCprgg
005100070116     d  VSCSQL
005200070116     d  Selected                      1A
005300070116
005400070116      /COPY QSYSINC/QRPGLESRC,QLGSORT
005500070116     d QLGKL                         16    DIM(MaxKey)
005600070116     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
005700070116     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
005800070116     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
005900070116     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
006000070116
006100070116      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
006200070116      /COPY QSYSINC/QRPGLESRC,QUSEC
006300061002     C*
006400950525     C* PULIZIA SUBFILE
006500070119     C                   SETON                                        20
006600950525     C                   WRITE     LRF0C03
006700070119     C                   SETOFF                                       20
006800070117     C                   Z-ADD     0             nrr
006900950525     C* CARICAMENTO SUBFILE
007000950525     C                   EXSR      RIES03
007100950525     C* SE NON E' STATO CARICATO NESSUN RECORD ESCO
007200950525     C     NRR           IFEQ      0
007300070119     c                   movel     fidg08ds      kpjbu
007400070112     c                   seton                                        lr
007500070112     C                   return
007600061002     C                   else
007700070119     c* accendo il 50 perchè forzo l'ordinamento  x rag. soc.
007800070119     c                   seton                                        50
007900070119     C                   EXSR      F11GES
008000070124     c                   z-add     1             rec1
008100070124     C                   EXSR      ctrs03
008200950525     C* EMISSIONE FORMATO
008300070119     c                   do        *hival
008400070119     C                   WRITE     LRF0Z04
008500950525     C                   EXFMT     LRF0C03
008600950525     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
008700950525     C                   CLEAR                   V2CMSG
008800950525     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
008900070112     C                   SETOFF                                       28
009000070115     C** F3 - FINE F12 - RITORNO
009100070115    1C                   IF        *inkc or *inkl
009200070119     c                   movel     fidg08ds      kpjbu
009300070112     C                   seton                                        lr
009400070112     C                   return
009500950525    1C                   ENDIF
009600070116     C* F11 = Cambia ordinamento autotrasp.
009700070116if  1C                   IF        *INKK
009800070116     C                   EXSR      F11GES
009900070116e   1C                   ENDIF
010000070115     c*
010100950525     C                   EXSR      CTRS03
010200061002     C   28              iter
010300061002     c                   enddo
010400950526     C* RITORNO ALLA VIDEATA INIZIALE (QUELLA DELLE SELEZIONI)
010500061002     c                   end
010600070112     C**********************************************************************
010700070112     C*--- CARICAMENTO SUBFILE ---------------------------------------*
010800070112     C**********************************************************************
010900070112     C     RIES03        BEGSR
011000950525     C                   Z-ADD     0             NRR               4 0
011100070115     c* per numero assoluto
011200070119     c                   if        d08nras<> 0
011201070720     c* CONSEGNE
011202070720     c                   if        d08tp = 'C'
011300070119     c     d08nras       setll     fiapd31l
011400070115     c                   else
011401070720     c* RITIRI
011402070720     c     d08nras       setll     fiapd51l
011403070720     c                   end
011404070720     c                   else
011500070115     c* per autotrasportatore
011501070720     c* CONSEGNE
011502070720     c                   if        d08tp = 'C'
011600070115     c     kapd          setll     fiapd32l
011601070720     c                   else
011602070720     c* RITIRI
011603070720     c     kapd          setll     fiapd52l
011700070115     c                   end
011701070720     c                   end
011800070112     c                   do        *hival
011900070115     c* per numero assoluto
012000070119     c                   if        d08nras<> 0
012001070720     c* CONSEGNE
012002070720     c                   if        d08tp = 'C'
012100070119     c     d08nras       reade     fiapd31l
012101070720     c                   if        %eof(fiapd31l)
012102070720     c                   leave
012103070720     c                   end
012104070720     c                   else
012105070720     c* RITIRI
012106070720     c     d08nras       reade     fiapd51l
012107070720     c                   if        %eof(fiapd51l)
012108070720     c                   leave
012109070720     c                   end
012110070720     c                   end
012500070115     c                   else
012501070720     c* CONSEGNE
012502070720     c                   if        d08tp = 'C'
012600070115     c                   if        d08pdr <> 0
012700070115     c     kapd          reade     fiapd32l
012800070112     c                   else
012900070119     c     d08tipa       reade     fiapd32l
013000070112     c                   end
013100070115     c                   if        %eof(fiapd32l)
013200070112     c                   leave
013300070112     c                   end
013301070720     c                   else
013302070720     c* RITIRI
013303070720     c                   if        d08pdr <> 0
013304070720     c     kapd          reade     fiapd52l
013305070720     c                   else
013306070720     c     d08tipa       reade     fiapd52l
013307070720     c                   end
013308070720     c                   if        %eof(fiapd52l)
013309070720     c                   leave
013310070720     c                   end
013400070115     c                   end
013401070720     c                   end
013402070720     c* CONSEGNE
013403070720     c                   if        d08tp = 'C'
013500070122     C* progressivo
013600070122     c                   if        d08prgg<>0  and apd3prgg<> d08prgg
013700070112     c                   iter
013800070112     c                   end
013900070122     C* annullati si/no
014000070122     c                   if        d08atb = 'N' and apd3atb <> ' '
014100070122     c                   iter
014200070122     c                   end
014300070112     c* controllo se aut di competenza p.o.
014400070115     c                   if        d08fgs <> 0 and d08pdr = 0
014500070115     c                   movel     apd3pdr       com03             3 0
014600070115     c                   if        com03 > d08fgs
014700070112     c                   leave
014800070112     c                   end
014900070112     c                   end
015000070112     c*
015100070112     C     kapd2         CHAIN     fiapd01l
015200070112     c                   if        not %found(fiapd01l) or
015300070112     c                             (%found(fiapd01l) and apdatb <> ' ')
015400070112     C                   iter
015500070112    1C                   ENDIF
015700070115     C                   MOVEL     apd3atb       VSCatb
015800070119     C                   MOVE      APD3nras      VShnras
016100070115     C                   MOVEL     apd3fpp       VSCfpp
016200070115     C                   MOVE      apd3dde       dtaiso
016300070112     C                   MOVE      dtaiso        vscdde
016400070119     C                   MOVE      apd3dscad     dtaiso
016500070119     C                   MOVE      dtaiso        vscscad
016600070123     C                   MOVEl     apd3cgi       vsccgi
016700070129     C                   MOVEl     apd3fgsg      vscfgsg
017000070119     C                   MOVE      apd3prgg      vscprgg
017100070115     C                   MOVE      apd3sql       vscsql
017101070720     c* RITIRI
017102070720     c                   else
017103070720     C* progressivo
017104070720     c                   if        d08prgg<>0  and apd5prgg<> d08prgg
017105070720     c                   iter
017106070720     c                   end
017107070720     C* annullati si/no
017108070720     c                   if        d08atb = 'N' and apd5atb <> ' '
017109070720     c                   iter
017110070720     c                   end
017111070720     c* controllo se aut di competenza p.o.
017112070720     c                   if        d08fgs <> 0 and d08pdr = 0
017113070720     c                   movel     apd5pdr       com03             3 0
017114070720     c                   if        com03 > d08fgs
017115070720     c                   leave
017116070720     c                   end
017117070720     c                   end
017118070720     c*
017119070720     C     kapd5         CHAIN     fiapd01l
017120070720     c                   if        not %found(fiapd01l) or
017121070720     c                             (%found(fiapd01l) and apdatb <> ' ')
017122070720     C                   iter
017123070720    1C                   ENDIF
017125070720     C                   MOVEL     apd5atb       VSCatb
017126070720     C                   MOVE      APD5nras      VShnras
017129070720     C                   MOVEL     apd5fpp       VSCfpp
017130070720     C                   MOVE      apd5dde       dtaiso
017131070720     C                   MOVE      dtaiso        vscdde
017132070720     C                   MOVE      apd5dscad     dtaiso
017133070720     C                   MOVE      dtaiso        vscscad
017134070720     C                   MOVEl     apd5cgi       vsccgi
017135070720     C                   MOVEl     apd5fgsg      vscfgsg
017138070720     C                   MOVE      apd5prgg      vscprgg
017139070720     C                   MOVE      apd5sql       vscsql
017140070720     c                   end
017200950525     C*
017201070720     C                   MOVE      APDPDR        VSCPDR
017202070720     C                   MOVEL     APDRSC        VSDPDR
017203070720     c                   clear                   vsdcgi
017204070720     c                   exsr      srcgi
017205070720     C                   CLEAR                   VSCSCE
017300950525     C                   ADD       1             NRR
017400950525     C                   WRITE     LRF0S03
017500950525    1C                   ENDDO
017600950525     C*
017700070117     C                   Z-ADD     nrr           STMREC1                        *n° rel sf x stampa
017800950525     C                   ENDSR
017900061002     C**********************************************************************
018000950525     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
018100061002     C**********************************************************************
018200950525     C     CTRS03        BEGSR
018300950526     C* LEGGO IL SFL PER SAPERE SE E' STATO SELEZIONATO ALMENO UN RECORD
018400070201     c                   setoff                                       21
018500070117     c                   do        *hival        x
018600070117     C     x             CHAIN     LRF0S03                            31
018700070112    1C                   if        *in31
018800070112     c                   leave
018900070112     c                   end
019000070112     C* SCE = "1" --> E' STATO SELEZIONATO UN RECORD DEL SFL
019100950526     C     VSCSCE        IFEQ      '1'
019200070112     C                   exsr      srfine
019300950526     C                   ENDIF
019400070201     C                   IF        V1cdes <> ' ' AND V1cdes <> VDESS
019500070201     c                   seton                                        21
019600070117   3aC                   IF        (*in50 = *off and v1cDes <= vsdpdr) or
019700070117     C                             (*in50 = *on  and v1Cpdr <= vscpdr)
019800070201     C                   MOVEL     V1cdes        VDESS
019900070117     C                   z-add     x             Rec1
020000070119     c                   setoff                                       21
020100070117     c                   leave
020200070117     C                   ENDIF
020300070201     C                   ENDIF
020400950526    1C                   ENDDO
020500950607     C*
020600070112     C                   ENDSR
020700070124     C**************************************************************************
020800070124     C* controllo e decodifico giro
020900070124     C**************************************************************************
021000070124     C     Srcgi         BEGSR
021100070124     C*
021200070129     c                   clear                   fidg09ds
021300070129     c                   eval      D09IOP0 = '001'
021400070129     c                   eval      d09ifgs = vscfgsg
021500070129     c                   eval      d09icgi = vsccgi
021600070129     c                   move      vscdde        dtaiso
021700070129     c                   move      dtaiso        d09idat
021800070720     c                   eval      d09itug = d08tp
022000070129     c                   eval      kpjbu = fidg09ds
022100070129     c                   call      'FIDG09R'
022200070129     c                   parm                    kpjba
022300070129     c                   eval      fidg09ds = kpjbu
022400070129     c                   if        d09oerr = '1'
022500070129     c                   eval      vsdcgi = *all'?'
022600070129     c                   else
022700070129     c                   eval      vsdcgi = d09odes
022800070129     c                   end
022900070124     C*
023000070124     C                   ENDSR
023100070112     C**********************************************************************
023200070112     C*--- fine ------------------------------------------------------*
023300070112     C**********************************************************************
023400070112     C     srfine        BEGSR
023500070112     C*
023600070115     c                   eval      d08pdr = vscpdr
023700070201     c                   eval      d08fppo= vscfpp
023800070116     c                   move      vscdde        dtaiso
023900070201     c                   move      dtaiso        d08ddeo
024000070119     c                   move      vscscad       dtaiso
024100070201     c                   move      dtaiso        d08scado
024200070129     c                   movel     vscfgsg       d08fgsgo
024300070129     c                   movel     vsccgi        d08cgio
024400070119     c                   move      vscprgg       d08prggo
024500070115     c                   move      vscsql        d08sqlo
024600070115     c                   movel     fidg08ds      kpjbu
024700070112     C                   SETON                                        LR
024800070112     c                   return
024900070112     C*
025000070112     C                   ENDSR
025100070116      *--------------------------------------------------------------------------------------------*
025200070116      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/Aut.
025300070116      *--------------------------------------------------------------------------------------------*
025400070116     C     F11GES        BEGSR
025500070117     C                   setoff                                       72
025600070201     C                   CLEAR                   vdess
025700070116      * IMPOSTO ENTRATA AD 'A'
025800070117     C                   EVAL      rec1 = 1
025900070116      *
026000070116     C                   CLEAR                   QLGSCB
026100070116     C                   CLEAR                   QLGSCB00
026200070116
026300070116     ** 1 campo chiave.
026400070116     C                   EVAL      QLGNBRK    = 1
026500070116
026600070116     C                   IF        *in50=*off
026700070116     ** l'aut. è a posizione 4, numerico, 7 byte, ascending.
026800070129     C                   EVAL      QLGSP      = 69
026900070116     C                   EVAL      QLGSS      = %SIZE(vscpdr)
027000070116     C                   EVAL      QLGDT      = Numerico
027100070116     C                   EVAL      QLGSO      = Ascendente
027200070116     C                   EVAL      QLGKL(1)   = QLGSKL
027300070116     C                   seton                                        50
027400070116     C                   ELSE
027500070116      *
027600070116     ** La ragione sociale è a posizione 45, 20 byte, char, ascending.
027700070129     C                   EVAL      QLGSP      = 4
027800070116     C                   EVAL      QLGSS      = %SIZE(vsdpdr)
027900070116     C                   EVAL      QLGDT      = Carattere
028000070116     C                   EVAL      QLGSO      = Ascendente
028100070116     C                   EVAL      QLGKL(1)   = QLGSKL
028200070116     C                   setoff                                       50
028300070116     C                   ENDIF
028400070116
028500070116      * Load other sort parameters.
028600070116     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
028700070116     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
028800070116     C                   EVAL      QLGRT     = 8
028900070116     C                   EVAL      QLGOKL    = 80
029000070116     C                   EVAL      QLGLKE    = 16
029100070116     C                   EVAL      QLGLSS    = 290
029200070116
029300070116      * Initialize Sort I/O API fields.
029400070116     C                   EVAL      QLGRL00  = QLGRL
029500070116     C                   EVAL      QLGRC00  = 1
029600070116     C                   CLEAR                   QUSEI
029700070116     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
029800070116
029900070116      * First step - Initialize the sort routine.
030000070116     C                   CALL      'QLGSORT'
030100070116     C                   PARM                    QLGSCB
030200070116     C                   PARM                    NotUsed
030300070116     C                   PARM                    NotUsed
030400070116     C                   PARM                    SizeList
030500070116     C                   PARM                    ReturnSize
030600070116     C                   PARM                    QUSEC
030700070116
030800070116      * Next step - Write records to I/O routine.
030900070116     C                   EVAL      QLGRT00 = Put
031000070116
031100070117     C                   DO        STMREC1       x                 4 0
031200070116
031300070116     C     x             CHAIN     LRF0S03
031400070116
031500070116     ** Solo le righe con Selected = 'Y' sono riordinate,
031600070116     ** quindi per fare un ordinamento di tutte le righe
031700070116     ** metto 'Y' sempre.
031800070116     C                   EVAL      Selected = 'Y'
031900070116
032000070116     C                   CLEAR                   QUSEI
032100070116     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
032200070116     C                   CALL      'QLGSRTIO'
032300070116     C                   PARM                    QLGSCB00
032400070116     C                   PARM                    SflRcd
032500070116     C                   PARM                    NotUsed
032600070116     C                   PARM                    SizeList
032700070116     C                   PARM                    NotUsed
032800070116     C                   PARM                    QUSEC
032900070116
033000070116     C                   ENDDO
033100070116
033200070116      * Next step - Signal end of input, clear subfile for reload.
033300070116     C                   EVAL      QLGRT00 = EndPut
033400070116     C                   CLEAR                   QUSEI
033500070116     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
033600070116
033700070116     C                   CALL      'QLGSRTIO'
033800070116     C                   PARM                    QLGSCB00
033900070116     C                   PARM                    SflRcd
034000070116     C                   PARM                    NotUsed
034100070116     C                   PARM                    SizeList
034200070116     C                   PARM                    NotUsed
034300070116     C                   PARM                    QUSEC
034400070116
034500070119     C                   SETON                                            20    *
034600070116     C                   WRITE     LRF0C03                                      *
034700070119     C                   SETOFF                                       20        *
034800070116     C                   WRITE     LRF0Z04                                      *
034900070116
035000070116      * Final step - Write the records back to the subfile.
035100070116     C                   EVAL      QLGRT00 = Get
035200070116
035300070116     C                   DO        STMREC1       x
035400070116     C                   CLEAR                   QUSEI
035500070116     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035600070116     C                   CALL      'QLGSRTIO'
035700070116     C                   PARM                    QLGSCB00
035800070116     C                   PARM                    NotUsed
035900070116     C                   PARM                    SflRcd
036000070116     C                   PARM                    QLGRL00
036100070116     C                   PARM                    NotUsed
036200070116     C                   PARM                    QUSEC
036300070116      *
036400070117     c                   z-add     x             nrr
036500070116     C                   WRITE     LRF0S03
036600070116     C                   ENDDO
036700070119     C                   SETOFF                                       20        *
036800070116
036900070116      *
037000070116     C                   ENDSR
037100070112     C**********************************************************************
037200070112     C*--- INZSR INIZIALE --------------------------------------------*
037300070112     C**********************************************************************
037400070112     C     *INZSR        BEGSR
037500061002     C*
037600061002     C     *ENTRY        PLIST
037700061002     C                   PARM                    KPJBA
037800070112      * Reperisco dati job
037900070112     c                   exsr      DatiJob
038000070116     c*
038100070116     c                   eval      v1cpgm = 'FIDG08R'
038200070112     c*
038300070112     c                   if        kpjbu = ' '
038400070115     c                   clear                   fidg08ds
038500070112     c                   else
038600070115     C                   MOVEL     KPJBU         fidg08ds
038700070119     c                   movel     d08rsc        v1cdes
038800070201     c                   end
038900070129     c                   clear                   d08fgsgo
039000070129     c                   clear                   d08cgio
039100070119     c                   clear                   d08prggo
039200070115     c                   clear                   d08sqlo
039300070119     c                   if        d08tipa= ' '
039400070119     c                   eval      d08tipa= 'A'
039500070112     c                   end
039600070115     c                   if        d08pdr = 0 and d08fgs <> 0
039700070115     c                   movel     d08fgs        compdr
039800070112     c                   end
039900070115     c                   if        d08pdr <>0
040000070115     c                   movel     d08pdr        compdr
040100070112     c                   end
040200061002
040300070112     c     Kapd          Klist
040400070119     c                   Kfld                    d08tipa
040500070112     c                   Kfld                    compdr            7 0
040600070112     c     Kapd2         Klist
040700070119     c                   Kfld                    apd3tipa
040800070115     c                   Kfld                    apd3pdr
040801070720     c     Kapd5         Klist
040802070720     c                   Kfld                    apd5tipa
040803070720     c                   Kfld                    apd5pdr
040900061002     C                   ENDSR
041000061002      *---------------------------------------------------------------*
041100061002      * Reperimento Dati del job (Utente/Operativi)                   *
041200061002      *---------------------------------------------------------------*
041300061002     c     DatiJob       BEGSR
041400061002      *
041500061002     c     *dtaara       define    §azute        azuteds
041600061002     c     *dtaara       define    §datiute      ddatiute
041700061002      *
041800061002     c                   in(E)     *dtaara
041900061002     c                   IF        %ERROR or RSUT = *blanks
042000061002     c                   clear                   Tibs34Ds
042100061002     c                   call      'TIBS34R'
042200061002     c                   parm                    Tibs34Ds
042300061002     c                   in        *dtaara
042400061002     c                   ENDIF
042500061002      *
042600061002     c                   ENDSR
