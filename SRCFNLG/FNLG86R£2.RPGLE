000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050418     ftigcp26l  uf   e           k disk    commit
000400050221     FFNARB01L  IF   E           K DISK
000500071026     FFnlbl02l  IF   E           K DISK
000600071026     FFnblp01l  IF   E           K DISK
000700050221     FFiAR501L  IF   E           K DISK
000800050221     FTABEL00F  IF   E           K DISK
000900161125     FWFGCS50F  O    E             DISK    USROPN
001000161125     FWFGCS51L  IF   E           K DISK    RENAME(WFGCS050:WFGCS1) USROPN
001100161125     FWFGCS52L  IF   E           K DISK    RENAME(WFGCS050:WFGCS2) USROPN
001200070629     Ffnart01L  IF   E           K DISK
001300071026     Ffnblt01L  IF   E           K DISK
001400141215     Ffnbrv07L  IF   E           K DISK
001500161111     Ffnfvv01l  IF   E           K DISK
001600050221     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001700050221     D FIL£6           S              3  0 DIM(30)
001800050221     D §FLE            S              3  0
001900050221     D WLBDA8          DS
002000050221     D  G08DAT                 1      8  0
002100050221     D  G08INV                 9     16  0
002200050221     D  G08ERR                17     17
002300050221     D  G08TGI                18     22  0
002400050221      *
002500161111     D RECBRV        E DS                  EXTNAME(FNBRV00F)
002600161111     D WSAVB         E DS                  EXTNAME(FNBRV00F)
002700161111     d                                     prefix(S_)
002800050221     d dar5gen       e ds
002900050221     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003000070629     d dskiere         ds
003100070629     d  sk1                    1    140  0 dim(20)
003200070629     d  sk2                  141    280  0 dim(20)
003300070629     d  sk3                  281    420  0 dim(20)
003400070629     d  sk4                  421    560  0 dim(20)
003500070629     d  sk5                  561    700  0 dim(20)
003600070629     d  skt                    1    700  0 dim(100)
003700071026     d dskieref        ds
003800071026     d  sk1f                   1    140  0 dim(20)
003900071026     d  sk2f                 141    280  0 dim(20)
004000071026     d  sk3f                 281    420  0 dim(20)
004100071026     d  sk4f                 421    560  0 dim(20)
004200071026     d  sk5f                 561    700  0 dim(20)
004300071026     d  sktf                   1    700  0 dim(100)
004400071026
004500070629
004600161007     D tigcp00ds     E DS                  extname(tigcp00f)
004700161007     D fnlgi1ds      E DS
004800050221     D CNCR80        E DS
004900050221     D KPJBA         E DS
005000161111     D DS7B          E DS
005100050221     d trul06ds      e ds
005200050221     d  lin                    1     90  0 dim(30)
005300050221     D FNLG87        E DS                  EXTNAME(FNLG87DS)
005400161111     D* REPERISCE DATI FOGLI
005500161111     D DSLV53        E DS                  EXTNAME(FNLV53DS)
005600050221      *
005700050221     D                SDS
005800050221     D  §PGM                   1     10
005900071011     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006000071011     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006100071011     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006200071011     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006300071011     D cmd5            S             66    DIM(01) CTDATA PERRCD(1)
006400071011     D cmd6            S             66    DIM(01) CTDATA PERRCD(1)
006500161111     D WLBDAT          DS
006600161111     D  G02DAT                 1      8  0
006700161111     D  G02INV                 9     16  0
006800161111     D  G02ERR                17     17
006900161111     D  G02TGI                18     22  0
007000071011     d comman          s             80
007100071011     d lenght          s             15  5 inz(80)
007200161111     d wdfv            S                   LIKE(d53dfv)
007300161111     d WSPG            S                   LIKE(FVVSPG)
007400161111     d Wnfv5           S                   LIKE(FVVnfv)
007500070629      *----------------------------------------------------------------
007600050221     C                   DO        30            I                 2 0
007700050221     c                   if        fil£6(i) = 0
007800050221     c                   leave
007900050221     c                   endif
008000050221     C                   Z-ADD     fil£6(i)      FLE               3 0
008100050221     C*
008200050221     c   01              z-add     10            fas
008300050221     c   03              z-add     35            fas
008400050221      * lettura giacenze
008500050221      *
008600050418     c     kgcp          setll     tigcp26l
008700000607     c                   do        *hival
008800050418     c     fle           reade     tigcp26l                             90
008900050413      *allocazione
009000050413     c                   if        *in90
009100050418     c     fle           reade(n)  tigcp26l
009200050413     c                   if        %eof
009300050413     c                   leave
009400050413     c                   end
009500050413     c                   iter
009600050413     c                   endif
009700050413      *
009800000607     c                   if        %eof
009900000607     c                   leave
010000000607     c                   end
010100050221      * se richiesto materiale da portare a magazzino la fase non può essere
010200050221      * maggiore di 26
010300050418     c                   if        gcpfas > 30 and *in01
010400050221     c                   leave
010500040507     c                   end
010600050221
010700050408      *solo progressivo 0
010800050408     c                   if        gcpfrg <> 0
010900050408     c                   iter
011000050408     c                   end
011100161007     c*
011200161007     c                   clear                   fnlgi1ds
011300161007     c                   eval      ilgi1agc=gcpagc
011400161007     c                   eval      ilgi1fgc=gcpfgc
011500161007     c                   eval      ilgi1ngc=gcpngc
011600161007     c                   movel     fnlgi1ds      kpjbu
011700161007     c                   call      'FNLGI1R'
011800161007     c                   parm                    kpjba
011900161007     c                   parm                    tigcp00ds
012000161007     c                   movel     kpjbu         fnlgi1ds
012100161007
012200050411      * in uscita dal magazzino
012300050221--2)>C     GCPFAS        IFEQ      35
012400050221     C     GCPDuma       ANDEQ     0
012500050221     C     D87RIS        ANDEQ     *BLANK
012600050221     C     *IN03         ANDEQ     '1'
012700161007     c     olgi1outmg    andne     ' '
012800050221      *
012900050221     C     GCPFAS        OREQ      35
013000050221     C     GCPDuma       ANDEQ     D87DAT
013100050221     C     D87RIS        ANDEQ     'R'
013200050221     C     *IN03         ANDEQ     '1'
013300161007     c     olgi1outmg    andne     ' '
013400050221      *
013500050411      * in uscita dal magazzino x resi
013600141110---->C*mm  GCPFAS        OReq      50
013700141107---->C     GCPFAS        ORge      50
013800050411---->C     GCPdis        andeq     'R'
013900050411     C     GCPDuma       ANDEQ     0
014000050411     C     D87RIS        ANDEQ     *BLANK
014100050411     C     *IN03         ANDEQ     '1'
014200161007     c     olgi1outmg    andne     ' '
014300050221      *
014400050411---->C     GCPdis        oreq      'R'
014500050411     C     GCPDuma       ANDEQ     D87DAT
014600050411     C     D87RIS        ANDEQ     'R'
014700050411     C     *IN03         ANDEQ     '1'
014800161007     c     olgi1outmg    andne     ' '
014900161007      *
015000050411      * in uscita dal magazzino x dirottamenti
015100141110---->C*mm  GCPFAS        OReq      40
015200141107---->C     GCPFAS        ORge      40
015300050411---->C     GCPdis        andeq     'D'
015400050411     C     GCPDuma       ANDEQ     0
015500050411     C     D87RIS        ANDEQ     *BLANK
015600050411     C     *IN03         ANDEQ     '1'
015700161007     c     olgi1outmg    andne     ' '
015800050411      *
015900141110---->C*mm  GCPFAS        OReq      50
016000141110---->C*mm  GCPdis        andeq     'D'
016100141110     C*mm  GCPDuma       ANDEQ     0
016200141110     C*mm  D87RIS        ANDEQ     *BLANK
016300141110     C*mm  *IN03         ANDEQ     '1'
016400050420      *
016500050411---->C     GCPdis        oreq      'D'
016600050411     C     GCPDuma       ANDEQ     D87DAT
016700050411     C     D87RIS        ANDEQ     'R'
016800050411     C     *IN03         ANDEQ     '1'
016900161007     c     olgi1outmg    andne     ' '
017000050411
017100050411      *merce da portare in magazzino
017200050413---->C     GCPFAS        ORle      30
017300050221     C     D87RIS        ANDEQ     *BLANK
017400050221     C     *IN01         ANDEQ     '1'
017500161007     c     olgi1amg      andeq     'S'
017600161007     C**   GCPDLMa       ANDEQ     0
017700050221      *
017800050221     C     GCPFAS        ORle      70
017900050221     C     GCPDLMa       ANDEQ     D87DAT
018000050221     C     D87RIS        ANDEQ     'R'
018100050221     C     *IN01         ANDEQ     '1'
018200050221      *
018300050221     C* PRELEVO DATI BOLLA
018400141119     C     KARB          CHAIN     FNARB01L
018500080115     c                   if        %found(fnarb01l)
018600050221     c                   clear                   dar5gen
018700050221     C     KAR5          CHAIN     FiAR501L
018800070629     c                   if        %found(fiar501l)
018900050221     c                   movel     ar5uni        dar5gen
019000050221     c                   end
019100141215     c                   clear                   znc               2 0
019200141216     C     Kart1         chain     Fnart01L
019300141215     c                   if        %found(fnart01l)
019400141216     C     Kbrv          setll     Fnbrv07L
019500141215     c                   do        *hival
019600141216     C     Kbrv          reade     Fnbrv07L
019700141215     c                   if        %eof(fnbrv07l)
019800141215     c                   leave
019900141215     c                   endif
020000141216     c                   if        brvdcs = 0 or brvnpg = 8
020100141215     c                   iter
020200141215     c                   else
020300141215     c                   move      brvznc        znc
020400141215     c                   leave
020500141215     c                   endif
020600141215     c                   enddo
020700141215     c                   endif
020800071026      * solo per merce da prelevare dal magazzino *IN03=on
020900071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
021000071026     c                   if        *in03
021100080220     c                   seton                                        31
021200071026     c                   if        gcpdis = 'D' or gcpdis = 'R'
021300071026     C     Karb          chain     fnlbl02l                           31
021400071026     C  n31Kblp          chain     fnblp01l                           31
021500071026     c                   endif
021600071026     c                   endif
021700071011      *scrittura wf con bolle da stampare successivamente
021800071011     c                   exsr      scriviwf
021900080115     c                   endif
022000050221     c
022100071026     C* AGGIORNO DATA DI STAMPA
022200071011     C     D87RIS        IFEQ      *BLANK
022300071011     C   01              MOVE      AMGUD8        GCPDLMa
022400071011     C   01              EXCEPT    AGG5
022500071011     C   03              MOVE      AMGUD8        GCPDuma
022600071011     C   03              EXCEPT    AGG0
022700071011     c                   if        gescomm = 'S'
022800071011     c                   commit
022900071011     c                   endif
023000071011
023100071011     C                   END
023200071011     C*
023300071011     C                   END
023400071011     C*
023500071011     c                   enddo
023600071011
023700071011     c                   enddo
023800071011      * eliminazione file di work
023900161125     c                   close     wfgcs50f
024000071011      * rilegge wf e produce il tabulato
024100071011     c                   exsr      stampa
024200071011      *
024300071011     c                   Movel(p)  cmd1(1)       comman
024400071011     c                   eval      lenght = %len(%trim(comman))
024500071011     c                   Call      'QCMDEXC'                            97
024600071011     c                   Parm                    comman
024700071011     c                   Parm                    lenght
024800161125     c   03              close     wfgcs51l
024900161125     c   01              close     wfgcs52l
025000071011     c   03              Movel(p)  cmd3(1)       comman
025100071011     c   01              Movel(p)  cmd5(1)       comman
025200071011     c                   eval      lenght = %len(%trim(comman))
025300071011     c                   Call      'QCMDEXC'                            97
025400071011     c                   Parm                    comman
025500071011     c                   Parm                    lenght
025600161007
025700161007     c                   clear                   fnlgi1ds
025800161007     c                   eval      ILGI1TLA= 'C'
025900161007     c                   movel     fnlgi1ds      kpjbu
026000161007     c                   call      'FNLGI1R'
026100161007     c                   parm                    kpjba
026200161111     c
026300161111     C                   CLEAR                   DSLV53
026400161111     C                   MOVEL     'C'           D53TLA
026500161111     C                   CALL      'FNLV53R'
026600161111     C                   PARM                    DSLV53
026700161111     C*
026800071011     c                   seton                                        lr
026900071011      *_________________________________________________________
027000071011     c     stampa        begsr
027100071011      *_________________________________________________________
027200071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
027300071011      * merce da prelevare o da portare a magazzino
027400161125     c   03*loval        setll     wfgcs51l
027500161125     c   01*loval        setll     wfgcs52l
027600071011     c                   do        *hival
027700161125     c   03              read      wfgcs51l                               98
027800161125     c   01              read      wfgcs52l                               98
027900071011     c   98              leave
028000071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
028100070629     c                   clear                   skt
028200071011     c                   if        gcsfns = 'S'
028300071015     C     Kart          setll     Fnart01L
028400070629     c                   z-add     0             ncl               3 0
028500071011     c                   do        *hival
028600071015     C     Kart          reade     Fnart01L
028700070629     c                   if        %eof(fnart01l) or ncl = 100
028800070629     c                   leave
028900070629     c                   endif
029000070629     c                   add       1             ncl
029100070629     c                   z-add     artnsc        skt(ncl)
029200070629     c                   enddo
029300070629     c                   endif
029400070629     c
029500071026      * bolla figlia
029600071026     c                   clear                   sktf
029700140728     c                   clear                   spuntato          8
029800140728     c                   clear                   spu               3 0
029900140728     c                   clear                   nospu             3 0
030000140718     c                   if        *in03
030100071026     C     Kblt          setll     Fnblt01L
030200071026     c                   z-add     0             ncl               3 0
030300071026     c                   do        *hival
030400071026     C     Kblt          reade     Fnblt01L
030500071026     c                   if        %eof(fnblt01l) or ncl = 100
030600140728     c                   select
030700140728     c                   when      spu >= 1 and nospu = 0
030800140728     c                   eval      spuntato = 'Totale'
030900140728     c                   when      spu >= 1 and nospu >= 1
031000140728     c                   eval      spuntato = 'Parziale'
031100140728     c                   other
031200140728     c                   clear                   spuntato
031300140728     c                   endsl
031400071026     c                   leave
031500071026     c                   endif
031600140718     c* se presente la data entrata vuol dire
031700140718     c* che è già stato etichettato il pacco con la nuova
031800140718     c* etichetta della figlia
031900140728     c                   if        bltdse <> 0
032000140728     c                   add       1             spu
032100140728     c                   else
032200140728     c                   add       1             nospu
032300140728     c                   end
032400140718     c*
032500140718     c                   if        gcsfnsf = 'S'
032600071026     c                   add       1             ncl
032700071026     c                   z-add     bltnsc        sktf(ncl)
032800071026     c                   endif
032900140718     c                   enddo
033000140718     c                   endif
033100071026     c
033200050221      *
033300071011     C   03GCsDIS        IFNE      SAVDIS
033400050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
033500050221     C                   MOVEL     '2D'          COD
033600050221     C                   MOVEL     *BLANKS       KEY
033700071011     C                   MOVEL     GCsDIS        KEY
033800050221     C     KTAB          CHAIN     TABEL                              50
033900050221     C  N50              MOVEL     TBLUNI        DESCR            20
034000050221     C   50              MOVEL     *ALL'?'       DESCR
034100071026     c                   setoff                                       05
034200071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
034300071026     c                   seton                                        05
034400071026     c                   endif
034500050221     C                   EXCEPT    TESTA
034600050221     C     D87RIS        IFEQ      'R'
034700050221     C                   EXCEPT    TESTAR
034800050221     C                   END
034900050221     C                   EXCEPT    TESTAT
035000050221     C                   SETON                                        99
035100071011     C                   MOVE      GCsDIS        SAVDIS            1
035200071011     C                   END
035300050221      *
035400050221     C  N99
035500050221     COR OF              DO
035600050221     C                   EXCEPT    TESTA
035700050221     C     D87RIS        IFEQ      'R'
035800050221     C                   EXCEPT    TESTAR
035900050221     C                   END
036000050221     C                   EXCEPT    TESTAT
036100050221     C                   END
036200050221     C  N99              SETON                                        99
036300050221      *
036400140718     c                   movel     gcsrsm        arbrsms          23
036500140718     c                   movel     gcsrsd        arbrsds          23
036600161125     c                   movel     gcszng        s30_zng          30
036700050221     C     *IN03         IFEQ      '1'
036800071011     C                   Z-ADD     GCsdeda       G08INV
036900050221     C                   MOVE      *ZEROS        G08DAT
037000050221     C                   MOVEL     '3'           G08ERR
037100050221     C                   CALL      'XSRDA8'
037200050221     C                   PARM                    WLBDA8
037300050221     C                   Z-ADD     G08DAT        STPDED            8 0
037400070522     C                   MOVEL     '2C'          COD
037500070522     C                   MOVEL     *BLANKS       KEY
037600071011     C                   MOVEL     GCscfg        KEY
037700070522     C     KTAB          CHAIN     TABEL                              50
037800071011     C  N50              MOVEL     TBLUNI        gcscfgd          29
037900071011     C   50              MOVEL     *ALL'?'       gcscfgd
038000070522     C                   END
038100050221     C                   EXCEPT    RIGA
038200070629      * stampa dettaglio dei segnacolli se non sequenziali
038300070629     c                   if        skt(1) > 0
038400070629     c                   except    segn1
038500070629     c                   endif
038600070629     c                   if        skt(21) > 0
038700070629     c                   except    segn2
038800070629     c                   endif
038900070629     c                   if        skt(41) > 0
039000070629     c                   except    segn3
039100070629     c                   endif
039200070629     c                   if        skt(61) > 0
039300070629     c                   except    segn4
039400070629     c                   endif
039500070629     c                   if        skt(81) > 0
039600070629     c                   except    segn5
039700070629     c                   endif
039800071026      * solo per merce da prelevare dal magazzino *IN03=ON
039900071026      * seconda riga dettaglio bolla figlia
040000071026      * testa se il campo spedizione è pieno per capire se è stato
040100071026      * agganciato il legame altrimenti stampa anomalia
040200071026     c   03              if        gcsnspf > 0
040300140718     c                   movel     gcsRSMf       gcsRSMfs         23
040400071026     C                   EXCEPT    RIGAf
040500071026      * stampa dettaglio dei segnacolli se non sequenziali
040600071026     c                   if        sktf(1) > 0
040700071026     c                   except    segn1f
040800071026     c                   endif
040900071026     c                   if        sktf(21) > 0
041000071026     c                   except    segn2f
041100071026     c                   endif
041200071026     c                   if        sktf(41) > 0
041300071026     c                   except    segn3f
041400071026     c                   endif
041500071026     c                   if        sktf(61) > 0
041600071026     c                   except    segn4f
041700071026     c                   endif
041800071026     c                   if        sktf(81) > 0
041900071026     c                   except    segn5f
042000071026     c                   endif
042100071026      * separazione
042200071026     C                   EXCEPT    separa
042300071026     c                   else
042400071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
042500071026     C                   EXCEPT    errRIGAf
042600071026     c                   endif
042700071026     c                   endif
042800071011      *
042900071011     c                   enddo
043000071011     c                   endsr
043100071011      *_________________________________________________________
043200071011     c     scriviwf      begsr
043300071011      *_________________________________________________________
043400161125     c                   clear                   wfgcs050
043500071011     c                   z-add     arbaas        GCSaas
043600071011     c                   z-add     arblnp        GCSlnp
043700071011     c                   z-add     arbnrs        GCSnrs
043800071011     c                   z-add     arbnsp        GCSnsp
043900071011     c                   z-add     arblna        GCSlna
044000071011     c                   move      arbrsm        GCSrsm
044100071011     c                   move      arbrsd        GCSrsd
044200141215     c                   if        arbznc <> znc and znc <> 0
044300141215     c                   z-add     znc           gcsznc
044400141215     c                   else
044500071011     c                   z-add     arbznc        GCSznc
044600141215     c                   endif
044700071011     c                   z-add     gcpfgc        GCSfgc
044800071011     c                   z-add     gcpngc        GCSngc
044900071011     c                   z-add     gcpdeda       GCSdeda
045000071011     c                   move      gcpdis        GCSdis
045100071011     c                   move      gcpcfg        GCScfg
045200170126     c
045300170126     c* se non è stato portato in mag giacenza, non stampo
045400170126     c*  l'ubicazine, altrimenti si
045500170126     c                   if        gcpdlma>0
045600071011     c                   movel     §ar5zng       GCSzng
045700170126     c                   endif
045800161111     c* se la bolla ha la data merce a mag giacenze, significa che
045900161111     c*  probabilmente non e' stata portata in giacenza
046000161111     c*  quindi al posto della ubicazione stampo la data e ora ultima spunta
046100161111     c*
046200170126     c**                 if        gcpdlma=0
046300161111     C     Kart1         chain     Fnart01L
046400161111     c                   if        %found(fnart01l)
046500161111     c                   exsr      Ricspu
046600161111     c                   endif
046700170126     c***                endif
046800161111     c
046900071011     c                   move      §ar5bva       GCSbva
047000071026     c                   z-add     arbfls        GCSfls
047100071011     c                   z-add     arbncd        GCSncd
047200071011     c                   z-add     arbnca        GCSnca
047300071011     c                   move      arbfns        GCSfns
047400071011     c                   z-add     arbncl        GCSncl
047500071011     c                   z-add     arbpkf        GCSpkf
047600071011     c                   z-add     arbvlf        GCSvlf
047700071026      * solo per merce da prelevare da magazzino *IN03=on
047800071026      *figlia se non trova legame o blp non scrive nulla
047900071026     c   03              if        not *in31
048000071026     c                   z-add     blpaas        GCSaasf
048100071026     c                   z-add     blplnp        GCSlnpf
048200071026     c                   z-add     blpnrs        GCSnrsf
048300071026     c                   z-add     blpnsp        GCSnspf
048400071026     c                   z-add     blplna        GCSlnaf
048500071026     c                   move      blprsm        GCSrsmf
048600071026     c                   move      blprsd        GCSrsdf
048700071026     c                   z-add     blpznc        GCSzncf
048800071026     c                   z-add     blpfls        GCSflsf
048900071026     c                   z-add     blpncd        GCSncdf
049000071026     c                   z-add     blpnca        GCSncaf
049100071026     c                   move      blpfns        GCSfnsf
049200071026     c                   z-add     blpncl        GCSnclf
049300071026     c                   z-add     blppkf        GCSpkff
049400071026     c                   z-add     blpvlf        GCSvlff
049500071026     c                   endif
049600161125     c                   write     wfgcs050
049700071011     c                   endsr
049800161111     C**************************************************************************
049900161111     C* cerco spunta del collo in qualsiasi categoria
050000161111     C**************************************************************************
050100161111     C     RICSPU        BEGSR
050200161111     C                   CLEAR                   Wimm
050300161111     C                   CLEAR                   WSAVB
050400161111     C                   CLEAR                   WDFV
050500161111     C                   CLEAR                   WTROV
050600161111     C                   CLEAR                   WSPG
050700161111     C                   CLEAR                   stadsf           10
050800161111RM*  C     kbrv          CHAIN     FNBRV07l                           33
050900161111RM*  C*
051000161111RM* 1C     *IN33         DOWEQ     *OFF
051100161111     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
051200161111     c                   clear                   dslv53
051300161111     C                   z-add     brvnFV        D53NFV
051400161111     C                   MOVEL     brvNPG        D53NPG
051500161111     C                   MOVEL     brvFGS        D53FGS
051600161111     C****               MOVEL     brvfle        D53flf
051700161111     C                   CALL      'FNLV53R'
051800161111     C                   PARM                    DSLV53
051900161111     C* ERRORE
052000161111    2C     D53ERR        IFeq      ' '
052100161111     c                   movel     brvdfs        brvimm           14 0
052200161111     c                   move      brvhms        brvimm
052300161111    3c                   select
052400161111     c* Se maggiore memorizzo
052500161111     c                   when      d53dfv>wdfv
052600161111     C                   MOVEL     RECBRV        WSAVB
052700161111     C                   MOVEL     brvimm        Wimm             14 0
052800161111     C                   MOVEL     d53DFV        WDFV
052900161111     C                   MOVEL     d53spg        WSPG
053000161111     C                   MOVEL     'S'           WTROV             1
053100161111     c
053200161111     c* Se uguale tengo la data ora immissione + alta
053300161111     c                   when      d53dfv=wdfv  and brvimm>wimm
053400161111     C                   MOVEL     brvimm        Wimm             14 0
053500161111     C                   MOVEL     RECBRV        WSAVB
053600161111     C                   MOVEL     d53DFV        WDFV
053700161111     C                   MOVEL     d53spg        WSPG
053800161111     C                   MOVEL     'S'           WTROV             1
053900161111     C                   eval      wimm=brvimm
054000161111     c* Se stessa data e ora immissione, tengo cat 3 o 4
054100161111     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
054200161111     C                   MOVEL     brvimm        Wimm             14 0
054300161111     C                   MOVEL     RECBRV        WSAVB
054400161111     C                   MOVEL     d53DFV        WDFV
054500161111     C                   MOVEL     d53spg        WSPG
054600161111     C                   MOVEL     'S'           WTROV             1
054700161111     C                   eval      wimm=brvimm
054800161111    3c                   endsl
054900161111    2c                   endif
055000161111     c
055100161111RM*  C     Kartd         reade     FNBRV07l                               33
055200161111    1c                   enddo
055300161111     c
055400161111     C     WTROV         IFEQ      'S'
055500161111     C                   MOVEL     WSAVB         RECBRV
055600161111     C                   ENDIF
055700161111     c
055800161111     c* Memorizzo nel campo ubicazione
055900161111     C* DECODIFICO LA CATEGORIA SPUNTA
056000161111    1C     BRVNPG        IFEQ      3
056100161111     C                   MOVEL     '7B'          COD
056200161111     C                   MOVEL(P)  wspg          KEY
056300161111     C     KTAB          CHAIN     TABEL                              32
056400161111     C  N32              MOVEL     TBLUNI        ds7b
056500161111     C   32              CLEAR                   ds7b
056600161111     C                   MOVEL     §7bdesb       ST2CAT            3
056700161111   X1C                   ELSE
056800161111     C                   MOVEL     '7N'          COD
056900161111     C                   MOVEL(P)  BRVNPG        KEY
057000161111     C     KTAB          CHAIN     TABEL                              32
057100161111     C  N32              MOVEL     TBLUNI        ST2CAT
057200161111     C   32              CLEAR                   ST2CAT
057300161111    1C                   ENDIF
057400161111     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
057500161111    1C                   IF        BRVNPG=4
057600161111     C                   Z-ADD     BRVNFV        WNFV5
057700161111     C     KFVV2         CHAIN(N)  FNFVV000                           32
057800161111     C  N32              eval      stadsf=fvvdsf
057900161111    1C                   ENDIF
058000161111     C* GIRO LA DATA
058100161111     C                   MOVE      WDFV          G02INV
058200161111     C                   MOVE      *ZEROS        G02DAT
058300161111     C                   MOVEL     3             G02ERR
058400161111     C                   CALL      'XSRDA8'
058500161111     C                   PARM                    WLBDAT
058600161111     C                   MOVEL     G02DAT        ST2DFV            4 0
058700161111     c* ora
058800161111     c                   clear                   st2hms            4 0
058900161111     c                   eval      st2hms=brvhms/100
059000161111     c
059100161111     c                   eval      GCSzng=%trim(gcszng)
059200161111     c                   if        %subst(gcszng:1:1)<>' '
059300161111     c                   eval      gcszng=%trim(gcszng)+'-'
059400161111     c                   endif
059500161111     c
059600161111     c                   if        brvnpg<>4
059700161111     c                   eval      GCSzng=%trim(gcszng)+
059800161111     c                             st2cat+
059900161111     c                             ' ' +%editw(st2dfv:'  /  ')+
060000161111     c                             ' '+%editw(st2hms:'  :  ')
060100161111     c                   else
060200161111     c                   eval      GCSzng=%trim(gcszng)+
060300161111     c                             st2cat+
060400161111     c                             ' ' +%editw(st2dfv:'  /  ')+
060500161111     c                             ' '+stadsf
060600161111     c                   endif
060700161111     c
060800161111     c
060900161111     c                   endsr
061000050221     C*---------------------------------------------------------------*
061100050221     C     *INZSR        BEGSR
061200050221     C*---------------------------------------------------------------*
061300050221     C     *ENTRY        PLIST
061400050221     C                   PARM                    KPJBA
061500050221     C                   MOVEL     KPJBU         FNLG87
061600050221      *
061700050322     c                   move      d87comm       gescomm           1
061800050221      *
061900050221     C                   Z-ADD     1             CODUT
062000050221     C                   CALL      'X§PARUT'
062100050221     C                   PARM                    UTEDSE
062200050221     C                   MOVEL     REC80         CNCR80
062300050221     C                   Z-ADD     0             SAVLNP            3 0
062400050221      *
062500050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
062600050221     C     SimTpP        COMP      '2'                                    02
062700170221     c* la filiale da elaborare non è più dal profilo utente ma passata in DS
062800170221     C*****              Z-ADD     SIMPOU        §FLE
062900170221     c                   if        d87fgs<=*zeros
063000170221     C                   Z-ADD     SIMPOU        §FLE
063100170221     c                   else
063200170221     C                   movel     d87fgs        §FLE
063300170221     c                   endif
063400050221      *
063500050221     C                   TIME                    WHHUDT           14 0
063600050221     C                   MOVEL     WHHUDT        ORA               6 0
063700050221     C                   MOVE      WHHUDT        GMAUD8            8 0
063800050221     C* GIRO UDATE
063900050221     C                   MOVE      GMAUD8        G08DAT
064000050221     C                   MOVE      *ZEROS        G08INV
064100050221     C                   MOVEL     *BLANK        G08ERR
064200050221     C                   CALL      'XSRDA8'
064300050221     C                   PARM                    WLBDA8
064400050221     C                   Z-ADD     G08INV        AMGUD8            8 0
064500050221     C*
064600050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
064700050221     C     D87RIS        IFEQ      'R'
064800050221     C                   MOVE      D87DAT        G08INV
064900050221     C                   MOVE      *ZEROS        G08DAT
065000050221     C                   MOVEL     3             G08ERR
065100050221     C                   CALL      'XSRDA8'
065200050221     C                   PARM                    WLBDA8
065300050221     C                   Z-ADD     G08DAT        GMADRI            8 0
065400050221     C                   END
065500050221     C*
065600071011      * genero file di work in qtemp
065700071011     c                   Movel(p)  cmd2(1)       comman
065800071011     c                   eval      lenght = %len(%trim(comman))
065900071011     c                   Call      'QCMDEXC'                            97
066000071011     c                   Parm                    comman
066100071011     c                   Parm                    lenght
066200161125     c                   open      wfgcs50f
066300050221     C     D87TST        IFEQ      '1'
066400071011      * merce da portare
066500050221     C                   SETON                                        01
066600071011     c                   Movel(p)  cmd6(1)       comman
066700071011     c                   eval      lenght = %len(%trim(comman))
066800071011     c                   Call      'QCMDEXC'                            97
066900071011     c                   Parm                    comman
067000071011     c                   Parm                    lenght
067100161125     c                   open      wfgcs52l
067200050221     C                   ELSE
067300071011      * merce da prelevare
067400050221     C                   SETON                                        03
067500071011     c                   Movel(p)  cmd4(1)       comman
067600071011     c                   eval      lenght = %len(%trim(comman))
067700071011     c                   Call      'QCMDEXC'                            97
067800071011     c                   Parm                    comman
067900071011     c                   Parm                    lenght
068000161125     c                   open      wfgcs51l
068100050221     C                   END
068200071011     c   97              except    errore
068300050221      *
068400050221     C* DEFINIZIONE CHIAVI
068500050221     C     KGCp          KLIST
068600050221     C                   KFLD                    FLE
068700050221     C                   KFLD                    FAS               3 0
068800161111RM*  C     KFVV2         KLIST
068900161111RM*  C                   KFLD                    BRVNPG
069000161111RM*  C                   KFLD                    WNFV5
069100161111RM*  C                   KFLD                    BRVFGS
069200161111RM*  C     KARTD         KLIST
069300161111RM*  C                   KFLD                    ARTFLS
069400161111RM*  C                   KFLD                    ARTLNA
069500161111RM*  C                   KFLD                    ARTNRS
069600161111RM*  C                   KFLD                    ARTNSC
069700050221     C*
069800050221     C     KARB          KLIST
069900050412     C                   KFLD                    GCPAASa
070000050412     C                   KFLD                    GCPLNPa
070100050412     C                   KFLD                    GCPNRSa
070200050412     C                   KFLD                    GCPNSPa
070300141215      *
070400141215     C     Kart1         KLIST
070500141216     C                   KFLD                    ARbAAS
070600141216     C                   KFLD                    ARbLNP
070700141216     C                   KFLD                    ARbNRS
070800141216     C                   KFLD                    ARbNSP
070900141216     C     Kbrv          KLIST
071000141219     C                   KFLD                    artFLS
071100141216     C                   KFLD                    artLNA
071200141216     C                   KFLD                    artNRS
071300141216     C                   KFLD                    artNSC
071400071026     C*
071500071026     C     Karbf         KLIST
071600071026     C                   KFLD                    arbAAS
071700071026     C                   KFLD                    arbLNP
071800071026     C                   KFLD                    arbNRS
071900071026     C                   KFLD                    arbNSP
072000071026     C*
072100071026     C     Kblp          KLIST
072200071026     C                   KFLD                    LBLAAN
072300071026     C                   KFLD                    LBLLPN
072400071026     C                   KFLD                    LBLNRN
072500071026     C                   KFLD                    LBLNSN
072600071026      *
072700071015     C     Kart          KLIST
072800071015     C                   KFLD                    GCsAAS
072900071015     C                   KFLD                    GCsLNP
073000071015     C                   KFLD                    GCsNRS
073100071015     C                   KFLD                    GCsNSP
073200050221     C*
073300071026     C     Kblt          KLIST
073400071026     C                   KFLD                    gcsAASf
073500071026     C                   KFLD                    gcsLNPf
073600071026     C                   KFLD                    gcsNRSf
073700071026     C                   KFLD                    gcsNSPf
073800071026     C*
073900071026     C     KAR5          KLIST
074000071026     C                   KFLD                    GCPAASa
074100071026     C                   KFLD                    GCPLNPa
074200071026     C                   KFLD                    GCPNRSa
074300071026     C                   KFLD                    GCPNSPa
074400071026     C                   KFLD                    trec5             3
074500071026     c                   move      'GEN'         trec5
074600050221     C*
074700050221     C     KTAB          KLIST
074800050221     C                   KFLD                    CODUT
074900050221     C                   KFLD                    COD               2
075000050221     C                   KFLD                    KEY               8
075100050221     C*
075200071026     c                   movel     *all'_'       lin198          198
075300050221     C***--------------------------------------------------------***
075400050221     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
075500050221     C***--------------------------------------------------------***
075600050221     c                   clear                   trul06ds
075700050221     c                   eval      d06cod = '£6'
075800050221     c                   movel     §fle          d06key
075900050221     c                   movel     trul06ds      kpjbu
076000050221     c                   call      'TRUL06R'
076100050221     c                   parm                    kpjba
076200050221     c                   movel     kpjbu         trul06ds
076300050221     c                   movea     lin           fil£6
076400050221     C                   ENDSR
076500050221     C*****************************************************************
076600050221     Otigcp000  E            AGG0
076700050221     O                       GCPDuma
076800050221     Otigcp000  E            AGG5
076900050221     O                       GCPDLMa
077000050221     OPRTF198   E    03      TESTA          1 02
077100050221     O                       RAGUT             +  0
077200170221     O                                           25 '   '
077300050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
077400050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
077500050221     O                                         +  0 'GIACENZE *'
077600050221     O                       §PGM               127
077700050221     O                       GMAUD8             147 '  /  /    '
077800050221     O                                         + 10 'PAG:'
077900050221     O                       PAGE          Z   +  1
078000050221     O          E    01      TESTA          1 02
078100050221     O                       RAGUT             +  0
078200170221     O                                           25 '   '
078300050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
078400050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
078500050221     O                                         +  0 'GIACENZE *'
078600050221     O                       §PGM               127
078700050221     O                       GMAUD8             147 '  /  /    '
078800050221     O                                         + 10 'PAG:'
078900050221     O                       PAGE          Z   +  1
079000050221     O          E            TESTA          1
079100050221     O                       KNSIF             +  0
079200050221     O                       KNMUS             +  0
079300050221     O                       ORA                147 '  :  :  '
079400050221     O          E    03      TESTA          1
079500050221     O                                         +  0 'CON DISPOSIZIONI DI:'
079600050221     O                       DESCR             +  1
079700071026     O          E    03 05   TESTA       1
079800071026     O                                         +  0 '* Prima riga bolla O'
079900071026     O                                         +  0 'riginale '
080000071026     O                                         +  0 'Seconda riga nuova '
080100071026     O                                         +  0 'Bolla *'
080200071026     O          E    03 05   TESTA          0
080300071026     O                                         +  0 '* Prima riga bolla O'
080400071026     O                                         +  0 'riginale '
080500071026     O                                         +  0 'Seconda riga nuova '
080600071026     O                                         +  0 'Bolla *'
080700071026     O          E    03 05   TESTA          0
080800071026     O                                         +  0 '* Prima riga bolla O'
080900071026     O                                         +  0 'riginale '
081000071026     O                                         +  0 'Seconda riga nuova '
081100071026     O                                         +  0 'Bolla *'
081200050221     O          E            TESTAR      1
081300050221     O                                         +  0 '---------  RISTAMPA'
081400050221     O                                         +  1 '---------  ELENCO DEL'
081500050221     O                       GMADRI            +  1 '  /  /    '
081600050221     O          E            TESTAT      2
081700071026     O                                            3 'LNP'
081800161125     O                                            7 'LNA'
081900161125     O                                           13 'ANNO'
082000161125     O                                           17 'SER'
082100161125     O                                           25 'SPEDIZ.'
082200161125     O                                           35 'Mittente'
082300161125     O                                           64 'Destinatario'
082400161125     O               03                          95 'Motivo di chiusura'
082500161125     O                                          112 'FlSgn'
082600161125     O                                          128 'Segnacol.Dal-Al'
082700161125     O                                          134 'Colli'
082800161125     O                                          144 'Peso Kg'
082900161125     O                                          149 'Zona'
083000161125     O               03                         160 'Data esec.'
083100161125     o                                          172 'Ubicazione'
083200161125     o                                          195 'Val'
083300161125     o                                          198 'Sp'
083400050221     O          E            RIGA        2
083500071026     O                       GCsLNP               3
083600161125     O                       GCsLNA               7
083700161125     O                       GCsAAS              13
083800161125     O                       GCsNRS              17
083900161125     O                       GCsNSP        Z     25
084000161125     O                       arbRSMs             50
084100170125     O                       arbRSDs             75
084200161125     O                       gcscfgd            106
084300161125     O                       gcsfls        Z    111
084400161125     O                       gcsNCD        Z    120
084500161125     O                                          121 '/'
084600161125     O                       gcsNCA        Z    128
084700161125     O                       gcsNCL        Z    134
084800161125     O                       gcspkf        2    144
084900161125     O                       gcsZNC             148
085000161125     O               03      STPDED             160 '  /  /    '
085100161125     O                       S30_zng            192
085200161125     O                       gcsbva             194
085300070629     O          E            segn1       1
085400070629     O                       sk1(1)        z   + 30
085500070629     O                       sk1(2)        z   +  1
085600070629     O                       sk1(3)        z   +  1
085700070629     O                       sk1(4)        z   +  1
085800070629     O                       sk1(5)        z   +  1
085900070629     O                       sk1(6)        z   +  1
086000070629     O                       sk1(7)        z   +  1
086100070629     O                       sk1(8)        z   +  1
086200070629     O                       sk1(9)        z   +  1
086300070629     O                       sk1(10)       z   +  1
086400070629     O                       sk1(11)       z   +  1
086500070629     O                       sk1(12)       z   +  1
086600070629     O                       sk1(13)       z   +  1
086700070629     O                       sk1(14)       z   +  1
086800070629     O                       sk1(15)       z   +  1
086900070629     O                       sk1(16)       z   +  1
087000070629     O                       sk1(17)       z   +  1
087100070629     O                       sk1(18)       z   +  1
087200070629     O                       sk1(19)       z   +  1
087300070629     O                       sk1(20)       z   +  1
087400070629     O          E            segn2       1
087500070629     O                       sk2(1)        z   + 30
087600070629     O                       sk2(2)        z   +  1
087700070629     O                       sk2(3)        z   +  1
087800070629     O                       sk2(4)        z   +  1
087900070629     O                       sk2(5)        z   +  1
088000070629     O                       sk2(6)        z   +  1
088100070629     O                       sk2(7)        z   +  1
088200070629     O                       sk2(8)        z   +  1
088300070629     O                       sk2(9)        z   +  1
088400070629     O                       sk2(10)       z   +  1
088500070629     O                       sk2(11)       z   +  1
088600070629     O                       sk2(12)       z   +  1
088700070629     O                       sk2(13)       z   +  1
088800070629     O                       sk2(14)       z   +  1
088900070629     O                       sk2(15)       z   +  1
089000070629     O                       sk2(16)       z   +  1
089100070629     O                       sk2(17)       z   +  1
089200070629     O                       sk2(18)       z   +  1
089300070629     O                       sk2(19)       z   +  1
089400070629     O                       sk2(20)       z   +  1
089500070629     O          E            segn3       1
089600070629     O                       sk3(1)        z   + 30
089700070629     O                       sk3(2)        z   +  1
089800070629     O                       sk3(3)        z   +  1
089900070629     O                       sk3(4)        z   +  1
090000070629     O                       sk3(5)        z   +  1
090100070629     O                       sk3(6)        z   +  1
090200070629     O                       sk3(7)        z   +  1
090300070629     O                       sk3(8)        z   +  1
090400070629     O                       sk3(9)        z   +  1
090500070629     O                       sk3(10)       z   +  1
090600070629     O                       sk3(11)       z   +  1
090700070629     O                       sk3(12)       z   +  1
090800070629     O                       sk3(13)       z   +  1
090900070629     O                       sk3(14)       z   +  1
091000070629     O                       sk3(15)       z   +  1
091100070629     O                       sk3(16)       z   +  1
091200070629     O                       sk3(17)       z   +  1
091300070629     O                       sk3(18)       z   +  1
091400070629     O                       sk3(19)       z   +  1
091500070629     O                       sk3(20)       z   +  1
091600070629     O          E            segn4       1
091700070629     O                       sk4(1)        z   + 30
091800070629     O                       sk4(2)        z   +  1
091900070629     O                       sk4(3)        z   +  1
092000070629     O                       sk4(4)        z   +  1
092100070629     O                       sk4(5)        z   +  1
092200070629     O                       sk4(6)        z   +  1
092300070629     O                       sk4(7)        z   +  1
092400070629     O                       sk4(8)        z   +  1
092500070629     O                       sk4(9)        z   +  1
092600070629     O                       sk4(10)       z   +  1
092700070629     O                       sk4(11)       z   +  1
092800070629     O                       sk4(12)       z   +  1
092900070629     O                       sk4(13)       z   +  1
093000070629     O                       sk4(14)       z   +  1
093100070629     O                       sk4(15)       z   +  1
093200070629     O                       sk4(16)       z   +  1
093300070629     O                       sk4(17)       z   +  1
093400070629     O                       sk4(18)       z   +  1
093500070629     O                       sk4(19)       z   +  1
093600070629     O                       sk4(20)       z   +  1
093700070629     O          E            segn5       1
093800070629     O                       sk5(1)        z   + 30
093900070629     O                       sk5(2)        z   +  1
094000070629     O                       sk5(3)        z   +  1
094100070629     O                       sk5(4)        z   +  1
094200070629     O                       sk5(5)        z   +  1
094300070629     O                       sk5(6)        z   +  1
094400070629     O                       sk5(7)        z   +  1
094500070629     O                       sk5(8)        z   +  1
094600070629     O                       sk5(9)        z   +  1
094700070629     O                       sk5(10)       z   +  1
094800070629     O                       sk5(11)       z   +  1
094900070629     O                       sk5(12)       z   +  1
095000070629     O                       sk5(13)       z   +  1
095100070629     O                       sk5(14)       z   +  1
095200070629     O                       sk5(15)       z   +  1
095300070629     O                       sk5(16)       z   +  1
095400070629     O                       sk5(17)       z   +  1
095500070629     O                       sk5(18)       z   +  1
095600070629     O                       sk5(19)       z   +  1
095700070629     O                       sk5(20)       z   +  1
095800071026     O          E            RIGAf       1
095900071026     O                       GCsLNPf              3
096000161125     O                       GCsLNAf              7
096100161125     O                       GCsAASf             13
096200161125     O                       GCsNRSf             17
096300161125     O                       GCsNSPf       Z     25
096400161125     O                       gcsRSMfs            50
096500170125     O                       gcsRSDf             87
096600161125     O                       gcsflsf       z    111
096700161125     O                       gcsNCDf       Z    120
096800161125     O                                          121 '/'
096900161125     O                       gcsNCAf       Z    128
097000161125     O                       spuntato           198
097100071026     O          E            segn1f      1
097200071026     O                       sk1f(1)       z   + 30
097300071026     O                       sk1f(2)       z   +  1
097400071026     O                       sk1f(3)       z   +  1
097500071026     O                       sk1f(4)       z   +  1
097600071026     O                       sk1f(5)       z   +  1
097700071026     O                       sk1f(6)       z   +  1
097800071026     O                       sk1f(7)       z   +  1
097900071026     O                       sk1f(8)       z   +  1
098000071026     O                       sk1f(9)       z   +  1
098100071026     O                       sk1f(10)      z   +  1
098200071026     O                       sk1f(11)      z   +  1
098300071026     O                       sk1f(12)      z   +  1
098400071026     O                       sk1f(13)      z   +  1
098500071026     O                       sk1f(14)      z   +  1
098600071026     O                       sk1f(15)      z   +  1
098700071026     O                       sk1f(16)      z   +  1
098800071026     O                       sk1f(17)      z   +  1
098900071026     O                       sk1f(18)      z   +  1
099000071026     O                       sk1f(19)      z   +  1
099100071026     O                       sk1f(20)      z   +  1
099200071026     O          E            segn2f      1
099300071026     O                       sk2f(1)       z   + 30
099400071026     O                       sk2f(2)       z   +  1
099500071026     O                       sk2f(3)       z   +  1
099600071026     O                       sk2f(4)       z   +  1
099700071026     O                       sk2f(5)       z   +  1
099800071026     O                       sk2f(6)       z   +  1
099900071026     O                       sk2f(7)       z   +  1
100000071026     O                       sk2f(8)       z   +  1
100100071026     O                       sk2f(9)       z   +  1
100200071026     O                       sk2f(10)      z   +  1
100300071026     O                       sk2f(11)      z   +  1
100400071026     O                       sk2f(12)      z   +  1
100500071026     O                       sk2f(13)      z   +  1
100600071026     O                       sk2f(14)      z   +  1
100700071026     O                       sk2f(15)      z   +  1
100800071026     O                       sk2f(16)      z   +  1
100900071026     O                       sk2f(17)      z   +  1
101000071026     O                       sk2f(18)      z   +  1
101100071026     O                       sk2f(19)      z   +  1
101200071026     O                       sk2f(20)      z   +  1
101300071026     O          E            segn3f      1
101400071026     O                       sk3f(1)       z   + 30
101500071026     O                       sk3f(2)       z   +  1
101600071026     O                       sk3f(3)       z   +  1
101700071026     O                       sk3f(4)       z   +  1
101800071026     O                       sk3f(5)       z   +  1
101900071026     O                       sk3f(6)       z   +  1
102000071026     O                       sk3f(7)       z   +  1
102100071026     O                       sk3f(8)       z   +  1
102200071026     O                       sk3f(9)       z   +  1
102300071026     O                       sk3f(10)      z   +  1
102400071026     O                       sk3f(11)      z   +  1
102500071026     O                       sk3f(12)      z   +  1
102600071026     O                       sk3f(13)      z   +  1
102700071026     O                       sk3f(14)      z   +  1
102800071026     O                       sk3f(15)      z   +  1
102900071026     O                       sk3f(16)      z   +  1
103000071026     O                       sk3f(17)      z   +  1
103100071026     O                       sk3f(18)      z   +  1
103200071026     O                       sk3f(19)      z   +  1
103300071026     O                       sk3f(20)      z   +  1
103400071026     O          E            segn4f      1
103500071026     O                       sk4f(1)       z   + 30
103600071026     O                       sk4f(2)       z   +  1
103700071026     O                       sk4f(3)       z   +  1
103800071026     O                       sk4f(4)       z   +  1
103900071026     O                       sk4f(5)       z   +  1
104000071026     O                       sk4f(6)       z   +  1
104100071026     O                       sk4f(7)       z   +  1
104200071026     O                       sk4f(8)       z   +  1
104300071026     O                       sk4f(9)       z   +  1
104400071026     O                       sk4f(10)      z   +  1
104500071026     O                       sk4f(11)      z   +  1
104600071026     O                       sk4f(12)      z   +  1
104700071026     O                       sk4f(13)      z   +  1
104800071026     O                       sk4f(14)      z   +  1
104900071026     O                       sk4f(15)      z   +  1
105000071026     O                       sk4f(16)      z   +  1
105100071026     O                       sk4f(17)      z   +  1
105200071026     O                       sk4f(18)      z   +  1
105300071026     O                       sk4f(19)      z   +  1
105400071026     O                       sk4f(20)      z   +  1
105500071026     O          E            segn5f      1
105600071026     O                       sk5f(1)       z   + 30
105700071026     O                       sk5f(2)       z   +  1
105800071026     O                       sk5f(3)       z   +  1
105900071026     O                       sk5f(4)       z   +  1
106000071026     O                       sk5f(5)       z   +  1
106100071026     O                       sk5f(6)       z   +  1
106200071026     O                       sk5f(7)       z   +  1
106300071026     O                       sk5f(8)       z   +  1
106400071026     O                       sk5f(9)       z   +  1
106500071026     O                       sk5f(10)      z   +  1
106600071026     O                       sk5f(11)      z   +  1
106700071026     O                       sk5f(12)      z   +  1
106800071026     O                       sk5f(13)      z   +  1
106900071026     O                       sk5f(14)      z   +  1
107000071026     O                       sk5f(15)      z   +  1
107100071026     O                       sk5f(16)      z   +  1
107200071026     O                       sk5f(17)      z   +  1
107300071026     O                       sk5f(18)      z   +  1
107400071026     O                       sk5f(19)      z   +  1
107500071026     O                       sk5f(20)      z   +  1
107600071026     O          E            separa      1
107700071026     o                       lin198             198
107800071026     O          E            errRIGAf    1
107900071026     O                                         + 25 '* Attenzione non trovata '
108000071026     O                                         +  0 'spedizione figlia verifi'
108100071026     O                                         +  0 'care i dati della bolla *'
108200071011     O          E            errore         1 02
108300071011     O                       RAGUT             +  0
108400170221     O                                           25 '   '
108500071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
108600071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
108700071011     O                                         +  0 'GIACENZE *'
108800071011     O                       §PGM               127
108900071011     O                       GMAUD8             147 '  /  /    '
109000071011     O                                         + 10 'PAG:'
109100071011     O                       PAGE          Z   +  1
109200071011     O          E            errore      2
109300071011     O                                         + 25 '* Attenzione errore in '
109400071011     O                                         +  0 'esecuzione programma  '
109500071011     O                                         +  0 'avvisare il CED *'
109600071011      *_________________________________________________________________________
109700071011**  CMD1
109800161125DLTF FILE(QTEMP/WFGCS50F)
109900071011**  CMD2
110000161125CRTDUPOBJ OBJ(WFGCS50F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
110100071011**  CMD3
110200161125DLTF FILE(QTEMP/WFGCS51L)
110300071011**  CMD4
110400161125CRTDUPOBJ OBJ(WFGCS51L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
110500071011**  CMD5
110600161125DLTF FILE(QTEMP/WFGCS52L)
110700071011**  CMD6
110800161125CRTDUPOBJ OBJ(WFGCS52L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
