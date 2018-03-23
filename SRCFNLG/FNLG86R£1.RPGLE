000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050418     ftigcp26l  uf   e           k disk    commit
000400050221     FFNARB01L  IF   E           K DISK
000500071026     FFnlbl02l  IF   E           K DISK
000600071026     FFnblp01l  IF   E           K DISK
000700050221     FFiAR501L  IF   E           K DISK
000800050221     FTABEL00F  IF   E           K DISK
000900071011     FWFGCS00F  O    E             DISK    USROPN
001000071011     FWFGCS01L  IF   E           K DISK    RENAME(WFGCS000:WFGCS1) USROPN
001100071011     FWFGCS02L  IF   E           K DISK    RENAME(WFGCS000:WFGCS2) USROPN
001200070629     Ffnart01L  IF   E           K DISK
001300071026     Ffnblt01L  IF   E           K DISK
001400141215     Ffnbrv07L  IF   E           K DISK
001500050221     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001600050221     D FIL£6           S              3  0 DIM(30)
001700050221     D §FLE            S              3  0
001800050221     D WLBDA8          DS
001900050221     D  G08DAT                 1      8  0
002000050221     D  G08INV                 9     16  0
002100050221     D  G08ERR                17     17
002200050221     D  G08TGI                18     22  0
002300050221      *
002400050221     d dar5gen       e ds
002500050221     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002600070629     d dskiere         ds
002700070629     d  sk1                    1    140  0 dim(20)
002800070629     d  sk2                  141    280  0 dim(20)
002900070629     d  sk3                  281    420  0 dim(20)
003000070629     d  sk4                  421    560  0 dim(20)
003100070629     d  sk5                  561    700  0 dim(20)
003200070629     d  skt                    1    700  0 dim(100)
003300071026     d dskieref        ds
003400071026     d  sk1f                   1    140  0 dim(20)
003500071026     d  sk2f                 141    280  0 dim(20)
003600071026     d  sk3f                 281    420  0 dim(20)
003700071026     d  sk4f                 421    560  0 dim(20)
003800071026     d  sk5f                 561    700  0 dim(20)
003900071026     d  sktf                   1    700  0 dim(100)
004000071026
004100070629
004200161007     D tigcp00ds     E DS                  extname(tigcp00f)
004300161007     D fnlgi1ds      E DS
004400050221     D CNCR80        E DS
004500050221     D KPJBA         E DS
004600050221     d trul06ds      e ds
004700050221     d  lin                    1     90  0 dim(30)
004800050221     D FNLG87        E DS                  EXTNAME(FNLG87DS)
004900050221      *
005000050221     D                SDS
005100050221     D  §PGM                   1     10
005200071011     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
005300071011     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
005400071011     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
005500071011     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
005600071011     D cmd5            S             66    DIM(01) CTDATA PERRCD(1)
005700071011     D cmd6            S             66    DIM(01) CTDATA PERRCD(1)
005800071011     d comman          s             80
005900071011     d lenght          s             15  5 inz(80)
006000070629      *----------------------------------------------------------------
006100050221     C                   DO        30            I                 2 0
006200050221     c                   if        fil£6(i) = 0
006300050221     c                   leave
006400050221     c                   endif
006500050221     C                   Z-ADD     fil£6(i)      FLE               3 0
006600050221     C*
006700050221     c   01              z-add     10            fas
006800050221     c   03              z-add     35            fas
006900050221      * lettura giacenze
007000050221      *
007100050418     c     kgcp          setll     tigcp26l
007200000607     c                   do        *hival
007300050418     c     fle           reade     tigcp26l                             90
007400050413      *allocazione
007500050413     c                   if        *in90
007600050418     c     fle           reade(n)  tigcp26l
007700050413     c                   if        %eof
007800050413     c                   leave
007900050413     c                   end
008000050413     c                   iter
008100050413     c                   endif
008200050413      *
008300000607     c                   if        %eof
008400000607     c                   leave
008500000607     c                   end
008600050221      * se richiesto materiale da portare a magazzino la fase non può essere
008700050221      * maggiore di 26
008800050418     c                   if        gcpfas > 30 and *in01
008900050221     c                   leave
009000040507     c                   end
009100050221
009200050408      *solo progressivo 0
009300050408     c                   if        gcpfrg <> 0
009400050408     c                   iter
009500050408     c                   end
009600161007     c*
009700161007     c                   clear                   fnlgi1ds
009800161007     c                   eval      ilgi1agc=gcpagc
009900161007     c                   eval      ilgi1fgc=gcpfgc
010000161007     c                   eval      ilgi1ngc=gcpngc
010100161007     c                   movel     fnlgi1ds      kpjbu
010200161007     c                   call      'FNLGI1R'
010300161007     c                   parm                    kpjba
010400161007     c                   parm                    tigcp00ds
010500161007     c                   movel     kpjbu         fnlgi1ds
010600161007
010700050411      * in uscita dal magazzino
010800050221--2)>C     GCPFAS        IFEQ      35
010900050221     C     GCPDuma       ANDEQ     0
011000050221     C     D87RIS        ANDEQ     *BLANK
011100050221     C     *IN03         ANDEQ     '1'
011200161007     c     olgi1outmg    andne     ' '
011300050221      *
011400050221     C     GCPFAS        OREQ      35
011500050221     C     GCPDuma       ANDEQ     D87DAT
011600050221     C     D87RIS        ANDEQ     'R'
011700050221     C     *IN03         ANDEQ     '1'
011800161007     c     olgi1outmg    andne     ' '
011900050221      *
012000050411      * in uscita dal magazzino x resi
012100141110---->C*mm  GCPFAS        OReq      50
012200141107---->C     GCPFAS        ORge      50
012300050411---->C     GCPdis        andeq     'R'
012400050411     C     GCPDuma       ANDEQ     0
012500050411     C     D87RIS        ANDEQ     *BLANK
012600050411     C     *IN03         ANDEQ     '1'
012700161007     c     olgi1outmg    andne     ' '
012800050221      *
012900050411---->C     GCPdis        oreq      'R'
013000050411     C     GCPDuma       ANDEQ     D87DAT
013100050411     C     D87RIS        ANDEQ     'R'
013200050411     C     *IN03         ANDEQ     '1'
013300161007     c     olgi1outmg    andne     ' '
013400161007      *
013500050411      * in uscita dal magazzino x dirottamenti
013600141110---->C*mm  GCPFAS        OReq      40
013700141107---->C     GCPFAS        ORge      40
013800050411---->C     GCPdis        andeq     'D'
013900050411     C     GCPDuma       ANDEQ     0
014000050411     C     D87RIS        ANDEQ     *BLANK
014100050411     C     *IN03         ANDEQ     '1'
014200161007     c     olgi1outmg    andne     ' '
014300050411      *
014400141110---->C*mm  GCPFAS        OReq      50
014500141110---->C*mm  GCPdis        andeq     'D'
014600141110     C*mm  GCPDuma       ANDEQ     0
014700141110     C*mm  D87RIS        ANDEQ     *BLANK
014800141110     C*mm  *IN03         ANDEQ     '1'
014900050420      *
015000050411---->C     GCPdis        oreq      'D'
015100050411     C     GCPDuma       ANDEQ     D87DAT
015200050411     C     D87RIS        ANDEQ     'R'
015300050411     C     *IN03         ANDEQ     '1'
015400161007     c     olgi1outmg    andne     ' '
015500050411
015600050411      *merce da portare in magazzino
015700050413---->C     GCPFAS        ORle      30
015800050221     C     D87RIS        ANDEQ     *BLANK
015900050221     C     *IN01         ANDEQ     '1'
016000161007     c     olgi1amg      andeq     'S'
016100161007     C**   GCPDLMa       ANDEQ     0
016200050221      *
016300050221     C     GCPFAS        ORle      70
016400050221     C     GCPDLMa       ANDEQ     D87DAT
016500050221     C     D87RIS        ANDEQ     'R'
016600050221     C     *IN01         ANDEQ     '1'
016700050221      *
016800050221     C* PRELEVO DATI BOLLA
016900141119     C     KARB          CHAIN     FNARB01L
017000080115     c                   if        %found(fnarb01l)
017100050221     c                   clear                   dar5gen
017200050221     C     KAR5          CHAIN     FiAR501L
017300070629     c                   if        %found(fiar501l)
017400050221     c                   movel     ar5uni        dar5gen
017500050221     c                   end
017600141215     c                   clear                   znc               2 0
017700141216     C     Kart1         chain     Fnart01L
017800141215     c                   if        %found(fnart01l)
017900141216     C     Kbrv          setll     Fnbrv07L
018000141215     c                   do        *hival
018100141216     C     Kbrv          reade     Fnbrv07L
018200141215     c                   if        %eof(fnbrv07l)
018300141215     c                   leave
018400141215     c                   endif
018500141216     c                   if        brvdcs = 0 or brvnpg = 8
018600141215     c                   iter
018700141215     c                   else
018800141215     c                   move      brvznc        znc
018900141215     c                   leave
019000141215     c                   endif
019100141215     c                   enddo
019200141215     c                   endif
019300071026      * solo per merce da prelevare dal magazzino *IN03=on
019400071026      * se dirottamento o reso reperisce riferimenti figlia da stampare
019500071026     c                   if        *in03
019600080220     c                   seton                                        31
019700071026     c                   if        gcpdis = 'D' or gcpdis = 'R'
019800071026     C     Karb          chain     fnlbl02l                           31
019900071026     C  n31Kblp          chain     fnblp01l                           31
020000071026     c                   endif
020100071026     c                   endif
020200071011      *scrittura wf con bolle da stampare successivamente
020300071011     c                   exsr      scriviwf
020400080115     c                   endif
020500050221     c
020600071026     C* AGGIORNO DATA DI STAMPA
020700071011     C     D87RIS        IFEQ      *BLANK
020800071011     C   01              MOVE      AMGUD8        GCPDLMa
020900071011     C   01              EXCEPT    AGG5
021000071011     C   03              MOVE      AMGUD8        GCPDuma
021100071011     C   03              EXCEPT    AGG0
021200071011     c                   if        gescomm = 'S'
021300071011     c                   commit
021400071011     c                   endif
021500071011
021600071011     C                   END
021700071011     C*
021800071011     C                   END
021900071011     C*
022000071011     c                   enddo
022100071011
022200071011     c                   enddo
022300071011      * eliminazione file di work
022400071011     c                   close     wfgcs00f
022500071011      * rilegge wf e produce il tabulato
022600071011     c                   exsr      stampa
022700071011      *
022800071011     c                   Movel(p)  cmd1(1)       comman
022900071011     c                   eval      lenght = %len(%trim(comman))
023000071011     c                   Call      'QCMDEXC'                            97
023100071011     c                   Parm                    comman
023200071011     c                   Parm                    lenght
023300071011     c   03              close     wfgcs01l
023400071011     c   01              close     wfgcs02l
023500071011     c   03              Movel(p)  cmd3(1)       comman
023600071011     c   01              Movel(p)  cmd5(1)       comman
023700071011     c                   eval      lenght = %len(%trim(comman))
023800071011     c                   Call      'QCMDEXC'                            97
023900071011     c                   Parm                    comman
024000071011     c                   Parm                    lenght
024100161007
024200161007     c                   clear                   fnlgi1ds
024300161007     c                   eval      ILGI1TLA= 'C'
024400161007     c                   movel     fnlgi1ds      kpjbu
024500161007     c                   call      'FNLGI1R'
024600161007     c                   parm                    kpjba
024700161007
024800071011     c                   seton                                        lr
024900071011      *_________________________________________________________
025000071011     c     stampa        begsr
025100071011      *_________________________________________________________
025200071011      * legge la v.l. opportuna x l'ordinamento a seconda che sia
025300071011      * merce da prelevare o da portare a magazzino
025400071011     c   03*loval        setll     wfgcs01l
025500071011     c   01*loval        setll     wfgcs02l
025600071011     c                   do        *hival
025700071011     c   03              read      wfgcs01l                               98
025800071011     c   01              read      wfgcs02l                               98
025900071011     c   98              leave
026000071011      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
026100070629     c                   clear                   skt
026200071011     c                   if        gcsfns = 'S'
026300071015     C     Kart          setll     Fnart01L
026400070629     c                   z-add     0             ncl               3 0
026500071011     c                   do        *hival
026600071015     C     Kart          reade     Fnart01L
026700070629     c                   if        %eof(fnart01l) or ncl = 100
026800070629     c                   leave
026900070629     c                   endif
027000070629     c                   add       1             ncl
027100070629     c                   z-add     artnsc        skt(ncl)
027200070629     c                   enddo
027300070629     c                   endif
027400070629     c
027500071026      * bolla figlia
027600071026     c                   clear                   sktf
027700140728     c                   clear                   spuntato          8
027800140728     c                   clear                   spu               3 0
027900140728     c                   clear                   nospu             3 0
028000140718     c                   if        *in03
028100071026     C     Kblt          setll     Fnblt01L
028200071026     c                   z-add     0             ncl               3 0
028300071026     c                   do        *hival
028400071026     C     Kblt          reade     Fnblt01L
028500071026     c                   if        %eof(fnblt01l) or ncl = 100
028600140728     c                   select
028700140728     c                   when      spu >= 1 and nospu = 0
028800140728     c                   eval      spuntato = 'Totale'
028900140728     c                   when      spu >= 1 and nospu >= 1
029000140728     c                   eval      spuntato = 'Parziale'
029100140728     c                   other
029200140728     c                   clear                   spuntato
029300140728     c                   endsl
029400071026     c                   leave
029500071026     c                   endif
029600140718     c* se presente la data entrata vuol dire
029700140718     c* che è già stato etichettato il pacco con la nuova
029800140718     c* etichetta della figlia
029900140728     c                   if        bltdse <> 0
030000140728     c                   add       1             spu
030100140728     c                   else
030200140728     c                   add       1             nospu
030300140728     c                   end
030400140718     c*
030500140718     c                   if        gcsfnsf = 'S'
030600071026     c                   add       1             ncl
030700071026     c                   z-add     bltnsc        sktf(ncl)
030800071026     c                   endif
030900140718     c                   enddo
031000140718     c                   endif
031100071026     c
031200050221      *
031300071011     C   03GCsDIS        IFNE      SAVDIS
031400050221     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
031500050221     C                   MOVEL     '2D'          COD
031600050221     C                   MOVEL     *BLANKS       KEY
031700071011     C                   MOVEL     GCsDIS        KEY
031800050221     C     KTAB          CHAIN     TABEL                              50
031900050221     C  N50              MOVEL     TBLUNI        DESCR            20
032000050221     C   50              MOVEL     *ALL'?'       DESCR
032100071026     c                   setoff                                       05
032200071026     c                   if        gcsdis  = 'D' or gcsdis = 'R'
032300071026     c                   seton                                        05
032400071026     c                   endif
032500050221     C                   EXCEPT    TESTA
032600050221     C     D87RIS        IFEQ      'R'
032700050221     C                   EXCEPT    TESTAR
032800050221     C                   END
032900050221     C                   EXCEPT    TESTAT
033000050221     C                   SETON                                        99
033100071011     C                   MOVE      GCsDIS        SAVDIS            1
033200071011     C                   END
033300050221      *
033400050221     C  N99
033500050221     COR OF              DO
033600050221     C                   EXCEPT    TESTA
033700050221     C     D87RIS        IFEQ      'R'
033800050221     C                   EXCEPT    TESTAR
033900050221     C                   END
034000050221     C                   EXCEPT    TESTAT
034100050221     C                   END
034200050221     C  N99              SETON                                        99
034300050221      *
034400140718     c                   movel     gcsrsm        arbrsms          23
034500140718     c                   movel     gcsrsd        arbrsds          23
034600050221     C     *IN03         IFEQ      '1'
034700071011     C                   Z-ADD     GCsdeda       G08INV
034800050221     C                   MOVE      *ZEROS        G08DAT
034900050221     C                   MOVEL     '3'           G08ERR
035000050221     C                   CALL      'XSRDA8'
035100050221     C                   PARM                    WLBDA8
035200050221     C                   Z-ADD     G08DAT        STPDED            8 0
035300070522     C                   MOVEL     '2C'          COD
035400070522     C                   MOVEL     *BLANKS       KEY
035500071011     C                   MOVEL     GCscfg        KEY
035600070522     C     KTAB          CHAIN     TABEL                              50
035700071011     C  N50              MOVEL     TBLUNI        gcscfgd          29
035800071011     C   50              MOVEL     *ALL'?'       gcscfgd
035900070522     C                   END
036000050221     C                   EXCEPT    RIGA
036100070629      * stampa dettaglio dei segnacolli se non sequenziali
036200070629     c                   if        skt(1) > 0
036300070629     c                   except    segn1
036400070629     c                   endif
036500070629     c                   if        skt(21) > 0
036600070629     c                   except    segn2
036700070629     c                   endif
036800070629     c                   if        skt(41) > 0
036900070629     c                   except    segn3
037000070629     c                   endif
037100070629     c                   if        skt(61) > 0
037200070629     c                   except    segn4
037300070629     c                   endif
037400070629     c                   if        skt(81) > 0
037500070629     c                   except    segn5
037600070629     c                   endif
037700071026      * solo per merce da prelevare dal magazzino *IN03=ON
037800071026      * seconda riga dettaglio bolla figlia
037900071026      * testa se il campo spedizione è pieno per capire se è stato
038000071026      * agganciato il legame altrimenti stampa anomalia
038100071026     c   03              if        gcsnspf > 0
038200140718     c                   movel     gcsRSMf       gcsRSMfs         23
038300071026     C                   EXCEPT    RIGAf
038400071026      * stampa dettaglio dei segnacolli se non sequenziali
038500071026     c                   if        sktf(1) > 0
038600071026     c                   except    segn1f
038700071026     c                   endif
038800071026     c                   if        sktf(21) > 0
038900071026     c                   except    segn2f
039000071026     c                   endif
039100071026     c                   if        sktf(41) > 0
039200071026     c                   except    segn3f
039300071026     c                   endif
039400071026     c                   if        sktf(61) > 0
039500071026     c                   except    segn4f
039600071026     c                   endif
039700071026     c                   if        sktf(81) > 0
039800071026     c                   except    segn5f
039900071026     c                   endif
040000071026      * separazione
040100071026     C                   EXCEPT    separa
040200071026     c                   else
040300071026     c                   if        gcsdis = 'D' or gcsdis = 'R'
040400071026     C                   EXCEPT    errRIGAf
040500071026     c                   endif
040600071026     c                   endif
040700071011      *
040800071011     c                   enddo
040900071011     c                   endsr
041000071011      *_________________________________________________________
041100071011     c     scriviwf      begsr
041200071011      *_________________________________________________________
041300071011     c                   clear                   wfgcs000
041400071011     c                   z-add     arbaas        GCSaas
041500071011     c                   z-add     arblnp        GCSlnp
041600071011     c                   z-add     arbnrs        GCSnrs
041700071011     c                   z-add     arbnsp        GCSnsp
041800071011     c                   z-add     arblna        GCSlna
041900071011     c                   move      arbrsm        GCSrsm
042000071011     c                   move      arbrsd        GCSrsd
042100141215     c                   if        arbznc <> znc and znc <> 0
042200141215     c                   z-add     znc           gcsznc
042300141215     c                   else
042400071011     c                   z-add     arbznc        GCSznc
042500141215     c                   endif
042600071011     c                   z-add     gcpfgc        GCSfgc
042700071011     c                   z-add     gcpngc        GCSngc
042800071011     c                   z-add     gcpdeda       GCSdeda
042900071011     c                   move      gcpdis        GCSdis
043000071011     c                   move      gcpcfg        GCScfg
043100071011     c                   movel     §ar5zng       GCSzng
043200071011     c                   move      §ar5bva       GCSbva
043300071026     c                   z-add     arbfls        GCSfls
043400071011     c                   z-add     arbncd        GCSncd
043500071011     c                   z-add     arbnca        GCSnca
043600071011     c                   move      arbfns        GCSfns
043700071011     c                   z-add     arbncl        GCSncl
043800071011     c                   z-add     arbpkf        GCSpkf
043900071011     c                   z-add     arbvlf        GCSvlf
044000071026      * solo per merce da prelevare da magazzino *IN03=on
044100071026      *figlia se non trova legame o blp non scrive nulla
044200071026     c   03              if        not *in31
044300071026     c                   z-add     blpaas        GCSaasf
044400071026     c                   z-add     blplnp        GCSlnpf
044500071026     c                   z-add     blpnrs        GCSnrsf
044600071026     c                   z-add     blpnsp        GCSnspf
044700071026     c                   z-add     blplna        GCSlnaf
044800071026     c                   move      blprsm        GCSrsmf
044900071026     c                   move      blprsd        GCSrsdf
045000071026     c                   z-add     blpznc        GCSzncf
045100071026     c                   z-add     blpfls        GCSflsf
045200071026     c                   z-add     blpncd        GCSncdf
045300071026     c                   z-add     blpnca        GCSncaf
045400071026     c                   move      blpfns        GCSfnsf
045500071026     c                   z-add     blpncl        GCSnclf
045600071026     c                   z-add     blppkf        GCSpkff
045700071026     c                   z-add     blpvlf        GCSvlff
045800071026     c                   endif
045900071011     c                   write     wfgcs000
046000071011     c                   endsr
046100050221     C*---------------------------------------------------------------*
046200050221     C     *INZSR        BEGSR
046300050221     C*---------------------------------------------------------------*
046400050221     C     *ENTRY        PLIST
046500050221     C                   PARM                    KPJBA
046600050221     C                   MOVEL     KPJBU         FNLG87
046700050221      *
046800050322     c                   move      d87comm       gescomm           1
046900050221      *
047000050221     C                   Z-ADD     1             CODUT
047100050221     C                   CALL      'X§PARUT'
047200050221     C                   PARM                    UTEDSE
047300050221     C                   MOVEL     REC80         CNCR80
047400050221     C                   Z-ADD     0             SAVLNP            3 0
047500050221      *
047600050221     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
047700050221     C     SimTpP        COMP      '2'                                    02
047800050221     C                   Z-ADD     SIMPOU        §FLE
047900050221      *
048000050221     C                   TIME                    WHHUDT           14 0
048100050221     C                   MOVEL     WHHUDT        ORA               6 0
048200050221     C                   MOVE      WHHUDT        GMAUD8            8 0
048300050221     C* GIRO UDATE
048400050221     C                   MOVE      GMAUD8        G08DAT
048500050221     C                   MOVE      *ZEROS        G08INV
048600050221     C                   MOVEL     *BLANK        G08ERR
048700050221     C                   CALL      'XSRDA8'
048800050221     C                   PARM                    WLBDA8
048900050221     C                   Z-ADD     G08INV        AMGUD8            8 0
049000050221     C*
049100050221      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
049200050221     C     D87RIS        IFEQ      'R'
049300050221     C                   MOVE      D87DAT        G08INV
049400050221     C                   MOVE      *ZEROS        G08DAT
049500050221     C                   MOVEL     3             G08ERR
049600050221     C                   CALL      'XSRDA8'
049700050221     C                   PARM                    WLBDA8
049800050221     C                   Z-ADD     G08DAT        GMADRI            8 0
049900050221     C                   END
050000050221     C*
050100071011      * genero file di work in qtemp
050200071011     c                   Movel(p)  cmd2(1)       comman
050300071011     c                   eval      lenght = %len(%trim(comman))
050400071011     c                   Call      'QCMDEXC'                            97
050500071011     c                   Parm                    comman
050600071011     c                   Parm                    lenght
050700071011     c                   open      wfgcs00f
050800050221     C     D87TST        IFEQ      '1'
050900071011      * merce da portare
051000050221     C                   SETON                                        01
051100071011     c                   Movel(p)  cmd6(1)       comman
051200071011     c                   eval      lenght = %len(%trim(comman))
051300071011     c                   Call      'QCMDEXC'                            97
051400071011     c                   Parm                    comman
051500071011     c                   Parm                    lenght
051600071011     c                   open      wfgcs02l
051700050221     C                   ELSE
051800071011      * merce da prelevare
051900050221     C                   SETON                                        03
052000071011     c                   Movel(p)  cmd4(1)       comman
052100071011     c                   eval      lenght = %len(%trim(comman))
052200071011     c                   Call      'QCMDEXC'                            97
052300071011     c                   Parm                    comman
052400071011     c                   Parm                    lenght
052500071011     c                   open      wfgcs01l
052600050221     C                   END
052700071011     c   97              except    errore
052800050221      *
052900050221     C* DEFINIZIONE CHIAVI
053000050221     C     KGCp          KLIST
053100050221     C                   KFLD                    FLE
053200050221     C                   KFLD                    FAS               3 0
053300050221     C*
053400050221     C     KARB          KLIST
053500050412     C                   KFLD                    GCPAASa
053600050412     C                   KFLD                    GCPLNPa
053700050412     C                   KFLD                    GCPNRSa
053800050412     C                   KFLD                    GCPNSPa
053900141215      *
054000141215     C     Kart1         KLIST
054100141216     C                   KFLD                    ARbAAS
054200141216     C                   KFLD                    ARbLNP
054300141216     C                   KFLD                    ARbNRS
054400141216     C                   KFLD                    ARbNSP
054500141216     C     Kbrv          KLIST
054600141219     C                   KFLD                    artFLS
054700141216     C                   KFLD                    artLNA
054800141216     C                   KFLD                    artNRS
054900141216     C                   KFLD                    artNSC
055000071026     C*
055100071026     C     Karbf         KLIST
055200071026     C                   KFLD                    arbAAS
055300071026     C                   KFLD                    arbLNP
055400071026     C                   KFLD                    arbNRS
055500071026     C                   KFLD                    arbNSP
055600071026     C*
055700071026     C     Kblp          KLIST
055800071026     C                   KFLD                    LBLAAN
055900071026     C                   KFLD                    LBLLPN
056000071026     C                   KFLD                    LBLNRN
056100071026     C                   KFLD                    LBLNSN
056200071026      *
056300071015     C     Kart          KLIST
056400071015     C                   KFLD                    GCsAAS
056500071015     C                   KFLD                    GCsLNP
056600071015     C                   KFLD                    GCsNRS
056700071015     C                   KFLD                    GCsNSP
056800050221     C*
056900071026     C     Kblt          KLIST
057000071026     C                   KFLD                    gcsAASf
057100071026     C                   KFLD                    gcsLNPf
057200071026     C                   KFLD                    gcsNRSf
057300071026     C                   KFLD                    gcsNSPf
057400071026     C*
057500071026     C     KAR5          KLIST
057600071026     C                   KFLD                    GCPAASa
057700071026     C                   KFLD                    GCPLNPa
057800071026     C                   KFLD                    GCPNRSa
057900071026     C                   KFLD                    GCPNSPa
058000071026     C                   KFLD                    trec5             3
058100071026     c                   move      'GEN'         trec5
058200050221     C*
058300050221     C     KTAB          KLIST
058400050221     C                   KFLD                    CODUT
058500050221     C                   KFLD                    COD               2
058600050221     C                   KFLD                    KEY               8
058700050221     C*
058800071026     c                   movel     *all'_'       lin198          198
058900050221     C***--------------------------------------------------------***
059000050221     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
059100050221     C***--------------------------------------------------------***
059200050221     c                   clear                   trul06ds
059300050221     c                   eval      d06cod = '£6'
059400050221     c                   movel     §fle          d06key
059500050221     c                   movel     trul06ds      kpjbu
059600050221     c                   call      'TRUL06R'
059700050221     c                   parm                    kpjba
059800050221     c                   movel     kpjbu         trul06ds
059900050221     c                   movea     lin           fil£6
060000050221     C                   ENDSR
060100050221     C*****************************************************************
060200050221     Otigcp000  E            AGG0
060300050221     O                       GCPDuma
060400050221     Otigcp000  E            AGG5
060500050221     O                       GCPDLMa
060600050221     OPRTF198   E    03      TESTA          1 02
060700050221     O                       RAGUT             +  0
060800050221     O                       SIMFEL              25
060900050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
061000050221     O                                         +  0 'PRELEVARE DAL MAGAZZINO '
061100050221     O                                         +  0 'GIACENZE *'
061200050221     O                       §PGM               127
061300050221     O                       GMAUD8             147 '  /  /    '
061400050221     O                                         + 10 'PAG:'
061500050221     O                       PAGE          Z   +  1
061600050221     O          E    01      TESTA          1 02
061700050221     O                       RAGUT             +  0
061800050221     O                       SIMFEL              25
061900050221     O                                         + 25 '* ELENCO SPEDIZIONI DA '
062000050221     O                                         +  0 'PORTARE NEL MAGAZZINO '
062100050221     O                                         +  0 'GIACENZE *'
062200050221     O                       §PGM               127
062300050221     O                       GMAUD8             147 '  /  /    '
062400050221     O                                         + 10 'PAG:'
062500050221     O                       PAGE          Z   +  1
062600050221     O          E            TESTA          1
062700050221     O                       KNSIF             +  0
062800050221     O                       KNMUS             +  0
062900050221     O                       ORA                147 '  :  :  '
063000050221     O          E    03      TESTA          1
063100050221     O                                         +  0 'CON DISPOSIZIONI DI:'
063200050221     O                       DESCR             +  1
063300071026     O          E    03 05   TESTA       1
063400071026     O                                         +  0 '* Prima riga bolla O'
063500071026     O                                         +  0 'riginale '
063600071026     O                                         +  0 'Seconda riga nuova '
063700071026     O                                         +  0 'Bolla *'
063800071026     O          E    03 05   TESTA          0
063900071026     O                                         +  0 '* Prima riga bolla O'
064000071026     O                                         +  0 'riginale '
064100071026     O                                         +  0 'Seconda riga nuova '
064200071026     O                                         +  0 'Bolla *'
064300071026     O          E    03 05   TESTA          0
064400071026     O                                         +  0 '* Prima riga bolla O'
064500071026     O                                         +  0 'riginale '
064600071026     O                                         +  0 'Seconda riga nuova '
064700071026     O                                         +  0 'Bolla *'
064800050221     O          E            TESTAR      1
064900050221     O                                         +  0 '---------  RISTAMPA'
065000050221     O                                         +  1 '---------  ELENCO DEL'
065100050221     O                       GMADRI            +  1 '  /  /    '
065200050221     O          E            TESTAT      2
065300071026     O                                            3 'LNP'
065400071026     O                                            8 'LNA'
065500071026     O                                           14 'ANNO'
065600071026     O                                           19 'SER.'
065700071026     O                                           27 'SPEDIZ.'
065800071026     O                                           37 'Mittente'
065900140718     O                                           66 'Destinatario'
066000140718     O               03                          97 'Motivo di chiusura'
066100140718     O                                          116 'F.Sgn'
066200140718     O                                          132 'Segnacol.Dal-Al'
066300140718     O                                          142 'Colli'
066400140718     O                                          152 'Peso Kg'
066500140718     O                                          157 'Zona'
066600140718     O               03                         168 'Data esec.'
066700140718     o                                          181 'Ubicazione'
066800140718     o                                          194 'Val'
066900140718     o                                          198 'Sp.'
067000050221     O          E            RIGA        2
067100071026     O                       GCsLNP               3
067200071026     O                       GCsLNA               8
067300071026     O                       GCsAAS              14
067400071026     O                       GCsNRS              18
067500071026     O                       GCsNSP        Z     27
067600141119     O                       arbRSMs             52
067700141119     O                       arbRSDs             77
067800141119     O                       gcscfgd            108
067900141119     O                       gcsfls        Z    115
068000141119     O                       gcsNCD        Z    124
068100140722     O                                          125 '/'
068200141119     O                       gcsNCA        Z    133
068300141119     O                       gcsNCL        Z    141
068400141119     O                       gcspkf        2    152
068500141119     O                       gcsZNC             156
068600140718     O               03      STPDED             168 '  /  /    '
068700140718     O                       gcszng             191
068800140718     O                       gcsbva             193
068900070629     O          E            segn1       1
069000070629     O                       sk1(1)        z   + 30
069100070629     O                       sk1(2)        z   +  1
069200070629     O                       sk1(3)        z   +  1
069300070629     O                       sk1(4)        z   +  1
069400070629     O                       sk1(5)        z   +  1
069500070629     O                       sk1(6)        z   +  1
069600070629     O                       sk1(7)        z   +  1
069700070629     O                       sk1(8)        z   +  1
069800070629     O                       sk1(9)        z   +  1
069900070629     O                       sk1(10)       z   +  1
070000070629     O                       sk1(11)       z   +  1
070100070629     O                       sk1(12)       z   +  1
070200070629     O                       sk1(13)       z   +  1
070300070629     O                       sk1(14)       z   +  1
070400070629     O                       sk1(15)       z   +  1
070500070629     O                       sk1(16)       z   +  1
070600070629     O                       sk1(17)       z   +  1
070700070629     O                       sk1(18)       z   +  1
070800070629     O                       sk1(19)       z   +  1
070900070629     O                       sk1(20)       z   +  1
071000070629     O          E            segn2       1
071100070629     O                       sk2(1)        z   + 30
071200070629     O                       sk2(2)        z   +  1
071300070629     O                       sk2(3)        z   +  1
071400070629     O                       sk2(4)        z   +  1
071500070629     O                       sk2(5)        z   +  1
071600070629     O                       sk2(6)        z   +  1
071700070629     O                       sk2(7)        z   +  1
071800070629     O                       sk2(8)        z   +  1
071900070629     O                       sk2(9)        z   +  1
072000070629     O                       sk2(10)       z   +  1
072100070629     O                       sk2(11)       z   +  1
072200070629     O                       sk2(12)       z   +  1
072300070629     O                       sk2(13)       z   +  1
072400070629     O                       sk2(14)       z   +  1
072500070629     O                       sk2(15)       z   +  1
072600070629     O                       sk2(16)       z   +  1
072700070629     O                       sk2(17)       z   +  1
072800070629     O                       sk2(18)       z   +  1
072900070629     O                       sk2(19)       z   +  1
073000070629     O                       sk2(20)       z   +  1
073100070629     O          E            segn3       1
073200070629     O                       sk3(1)        z   + 30
073300070629     O                       sk3(2)        z   +  1
073400070629     O                       sk3(3)        z   +  1
073500070629     O                       sk3(4)        z   +  1
073600070629     O                       sk3(5)        z   +  1
073700070629     O                       sk3(6)        z   +  1
073800070629     O                       sk3(7)        z   +  1
073900070629     O                       sk3(8)        z   +  1
074000070629     O                       sk3(9)        z   +  1
074100070629     O                       sk3(10)       z   +  1
074200070629     O                       sk3(11)       z   +  1
074300070629     O                       sk3(12)       z   +  1
074400070629     O                       sk3(13)       z   +  1
074500070629     O                       sk3(14)       z   +  1
074600070629     O                       sk3(15)       z   +  1
074700070629     O                       sk3(16)       z   +  1
074800070629     O                       sk3(17)       z   +  1
074900070629     O                       sk3(18)       z   +  1
075000070629     O                       sk3(19)       z   +  1
075100070629     O                       sk3(20)       z   +  1
075200070629     O          E            segn4       1
075300070629     O                       sk4(1)        z   + 30
075400070629     O                       sk4(2)        z   +  1
075500070629     O                       sk4(3)        z   +  1
075600070629     O                       sk4(4)        z   +  1
075700070629     O                       sk4(5)        z   +  1
075800070629     O                       sk4(6)        z   +  1
075900070629     O                       sk4(7)        z   +  1
076000070629     O                       sk4(8)        z   +  1
076100070629     O                       sk4(9)        z   +  1
076200070629     O                       sk4(10)       z   +  1
076300070629     O                       sk4(11)       z   +  1
076400070629     O                       sk4(12)       z   +  1
076500070629     O                       sk4(13)       z   +  1
076600070629     O                       sk4(14)       z   +  1
076700070629     O                       sk4(15)       z   +  1
076800070629     O                       sk4(16)       z   +  1
076900070629     O                       sk4(17)       z   +  1
077000070629     O                       sk4(18)       z   +  1
077100070629     O                       sk4(19)       z   +  1
077200070629     O                       sk4(20)       z   +  1
077300070629     O          E            segn5       1
077400070629     O                       sk5(1)        z   + 30
077500070629     O                       sk5(2)        z   +  1
077600070629     O                       sk5(3)        z   +  1
077700070629     O                       sk5(4)        z   +  1
077800070629     O                       sk5(5)        z   +  1
077900070629     O                       sk5(6)        z   +  1
078000070629     O                       sk5(7)        z   +  1
078100070629     O                       sk5(8)        z   +  1
078200070629     O                       sk5(9)        z   +  1
078300070629     O                       sk5(10)       z   +  1
078400070629     O                       sk5(11)       z   +  1
078500070629     O                       sk5(12)       z   +  1
078600070629     O                       sk5(13)       z   +  1
078700070629     O                       sk5(14)       z   +  1
078800070629     O                       sk5(15)       z   +  1
078900070629     O                       sk5(16)       z   +  1
079000070629     O                       sk5(17)       z   +  1
079100070629     O                       sk5(18)       z   +  1
079200070629     O                       sk5(19)       z   +  1
079300070629     O                       sk5(20)       z   +  1
079400071026     O          E            RIGAf       1
079500071026     O                       GCsLNPf              3
079600071026     O                       GCsLNAf              8
079700071026     O                       GCsAASf             14
079800071026     O                       GCsNRSf             18
079900071026     O                       GCsNSPf       Z     27
080000140718     O                       gcsRSMfs            52
080100140718     O                       gcsRSDf             89
080200140718     O                       gcsflsf       z    115
080300140718     O                       gcsNCDf       Z    124
080400140722     O                                          125 '/'
080500140718     O                       gcsNCAf       Z    133
080600140718     O                       spuntato           197
080700071026     O          E            segn1f      1
080800071026     O                       sk1f(1)       z   + 30
080900071026     O                       sk1f(2)       z   +  1
081000071026     O                       sk1f(3)       z   +  1
081100071026     O                       sk1f(4)       z   +  1
081200071026     O                       sk1f(5)       z   +  1
081300071026     O                       sk1f(6)       z   +  1
081400071026     O                       sk1f(7)       z   +  1
081500071026     O                       sk1f(8)       z   +  1
081600071026     O                       sk1f(9)       z   +  1
081700071026     O                       sk1f(10)      z   +  1
081800071026     O                       sk1f(11)      z   +  1
081900071026     O                       sk1f(12)      z   +  1
082000071026     O                       sk1f(13)      z   +  1
082100071026     O                       sk1f(14)      z   +  1
082200071026     O                       sk1f(15)      z   +  1
082300071026     O                       sk1f(16)      z   +  1
082400071026     O                       sk1f(17)      z   +  1
082500071026     O                       sk1f(18)      z   +  1
082600071026     O                       sk1f(19)      z   +  1
082700071026     O                       sk1f(20)      z   +  1
082800071026     O          E            segn2f      1
082900071026     O                       sk2f(1)       z   + 30
083000071026     O                       sk2f(2)       z   +  1
083100071026     O                       sk2f(3)       z   +  1
083200071026     O                       sk2f(4)       z   +  1
083300071026     O                       sk2f(5)       z   +  1
083400071026     O                       sk2f(6)       z   +  1
083500071026     O                       sk2f(7)       z   +  1
083600071026     O                       sk2f(8)       z   +  1
083700071026     O                       sk2f(9)       z   +  1
083800071026     O                       sk2f(10)      z   +  1
083900071026     O                       sk2f(11)      z   +  1
084000071026     O                       sk2f(12)      z   +  1
084100071026     O                       sk2f(13)      z   +  1
084200071026     O                       sk2f(14)      z   +  1
084300071026     O                       sk2f(15)      z   +  1
084400071026     O                       sk2f(16)      z   +  1
084500071026     O                       sk2f(17)      z   +  1
084600071026     O                       sk2f(18)      z   +  1
084700071026     O                       sk2f(19)      z   +  1
084800071026     O                       sk2f(20)      z   +  1
084900071026     O          E            segn3f      1
085000071026     O                       sk3f(1)       z   + 30
085100071026     O                       sk3f(2)       z   +  1
085200071026     O                       sk3f(3)       z   +  1
085300071026     O                       sk3f(4)       z   +  1
085400071026     O                       sk3f(5)       z   +  1
085500071026     O                       sk3f(6)       z   +  1
085600071026     O                       sk3f(7)       z   +  1
085700071026     O                       sk3f(8)       z   +  1
085800071026     O                       sk3f(9)       z   +  1
085900071026     O                       sk3f(10)      z   +  1
086000071026     O                       sk3f(11)      z   +  1
086100071026     O                       sk3f(12)      z   +  1
086200071026     O                       sk3f(13)      z   +  1
086300071026     O                       sk3f(14)      z   +  1
086400071026     O                       sk3f(15)      z   +  1
086500071026     O                       sk3f(16)      z   +  1
086600071026     O                       sk3f(17)      z   +  1
086700071026     O                       sk3f(18)      z   +  1
086800071026     O                       sk3f(19)      z   +  1
086900071026     O                       sk3f(20)      z   +  1
087000071026     O          E            segn4f      1
087100071026     O                       sk4f(1)       z   + 30
087200071026     O                       sk4f(2)       z   +  1
087300071026     O                       sk4f(3)       z   +  1
087400071026     O                       sk4f(4)       z   +  1
087500071026     O                       sk4f(5)       z   +  1
087600071026     O                       sk4f(6)       z   +  1
087700071026     O                       sk4f(7)       z   +  1
087800071026     O                       sk4f(8)       z   +  1
087900071026     O                       sk4f(9)       z   +  1
088000071026     O                       sk4f(10)      z   +  1
088100071026     O                       sk4f(11)      z   +  1
088200071026     O                       sk4f(12)      z   +  1
088300071026     O                       sk4f(13)      z   +  1
088400071026     O                       sk4f(14)      z   +  1
088500071026     O                       sk4f(15)      z   +  1
088600071026     O                       sk4f(16)      z   +  1
088700071026     O                       sk4f(17)      z   +  1
088800071026     O                       sk4f(18)      z   +  1
088900071026     O                       sk4f(19)      z   +  1
089000071026     O                       sk4f(20)      z   +  1
089100071026     O          E            segn5f      1
089200071026     O                       sk5f(1)       z   + 30
089300071026     O                       sk5f(2)       z   +  1
089400071026     O                       sk5f(3)       z   +  1
089500071026     O                       sk5f(4)       z   +  1
089600071026     O                       sk5f(5)       z   +  1
089700071026     O                       sk5f(6)       z   +  1
089800071026     O                       sk5f(7)       z   +  1
089900071026     O                       sk5f(8)       z   +  1
090000071026     O                       sk5f(9)       z   +  1
090100071026     O                       sk5f(10)      z   +  1
090200071026     O                       sk5f(11)      z   +  1
090300071026     O                       sk5f(12)      z   +  1
090400071026     O                       sk5f(13)      z   +  1
090500071026     O                       sk5f(14)      z   +  1
090600071026     O                       sk5f(15)      z   +  1
090700071026     O                       sk5f(16)      z   +  1
090800071026     O                       sk5f(17)      z   +  1
090900071026     O                       sk5f(18)      z   +  1
091000071026     O                       sk5f(19)      z   +  1
091100071026     O                       sk5f(20)      z   +  1
091200071026     O          E            separa      1
091300071026     o                       lin198             198
091400071026     O          E            errRIGAf    1
091500071026     O                                         + 25 '* Attenzione non trovata '
091600071026     O                                         +  0 'spedizione figlia verifi'
091700071026     O                                         +  0 'care i dati della bolla *'
091800071011     O          E            errore         1 02
091900071011     O                       RAGUT             +  0
092000071011     O                       SIMFEL              25
092100071011     O                                         + 25 '* ELENCO SPEDIZIONI DA '
092200071011     O                                         +  0 'PORTARE NEL MAGAZZINO '
092300071011     O                                         +  0 'GIACENZE *'
092400071011     O                       §PGM               127
092500071011     O                       GMAUD8             147 '  /  /    '
092600071011     O                                         + 10 'PAG:'
092700071011     O                       PAGE          Z   +  1
092800071011     O          E            errore      2
092900071011     O                                         + 25 '* Attenzione errore in '
093000071011     O                                         +  0 'esecuzione programma  '
093100071011     O                                         +  0 'avvisare il CED *'
093200071011      *_________________________________________________________________________
093300071011**  CMD1
093400071011DLTF FILE(QTEMP/WFGCS00F)
093500071011**  CMD2
093600071011CRTDUPOBJ OBJ(WFGCS00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
093700071011**  CMD3
093800071011DLTF FILE(QTEMP/WFGCS01L)
093900071011**  CMD4
094000071011CRTDUPOBJ OBJ(WFGCS01L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
094100071011**  CMD5
094200071011DLTF FILE(QTEMP/WFGCS02L)
094300071011**  CMD6
094400071011CRTDUPOBJ OBJ(WFGCS02L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
