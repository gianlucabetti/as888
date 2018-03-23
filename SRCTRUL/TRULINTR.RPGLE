000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200091215     H* TRULINTR *---------------------------------------------------*
000300091215     H*   - Determina intestatario contrassegno
000400060704     H*--------------------------------------------------------------*
000500060704     ftabel00f  if   e           k disk
000501091215     ffiar501l  if   e           k disk
000700090706     D MSG             S             78    DIM(12) CTDATA PERRCD(1)
000800011019
001000060704     d ds1a          e ds
001400071130     d kpjba         e ds
001500091215     d trulINTDS     e ds
001601091215     d dar5gen       e ds
001602091215     D TIBS69DS      E DS
001603091215     D ds_cnaco      E DS                  extname(CNACO00F)
001604091215     D ds_cnind      E DS                  extname(CNIND00F)
001605091215     D ds_cnclp      E DS                  extname(CNCLP00F)
001606091215     D ds_fncls      E DS                  extname(FNCLS00F)
001800011030
001900091215     d §kut            s                   like(tblkut) inz(1)
002100091215     d COD             s                   like(tblcod)
002300091215     d KEY             s                   like(tblkey)
002301091215     d KAr5Trd         s                   like(Ar5Trd) inz('GEN')
002302091215     D wtibs69r        s              1
002303091216     D w_cdi           s                   like(§ar5cdi)
002304091216     D ds1ap           s                   like(ds1a)
002400011019
002500011019      *----------------------------------------------------------------
002600060704     C     KTAB          KLIST
002700060704     C                   KFLD                    §kut
002800060704     C                   KFLD                    COD
002900060704     C                   KFLD                    KEY
002901091215     c     KFiar5        Klist
002902091215     c                   Kfld                    iintAas
002903091215     c                   Kfld                    iintLnp
002904091215     c                   Kfld                    iintNrs
002905091215     c                   Kfld                    iintNsp
002906091215     c                   Kfld                    KAr5Trd
003000060704      *----------------------------------------------------------------
003100011019
003200011029     c     *entry        plist
003300091215     c                   parm                    trulintDS
003400091216     c                   parm                    DS1AP
003500011106
003600011019
003700091215     c                   clear                   ointerr
003800091215     c                   clear                   ointmsg
003900091215     c                   clear                   ointrsci
004300060704     C* IMPOSTAZIONI
004400060704     C                   EXSR      IMPO
004500011019
004600011019      * Se Tipo lancio 'C' --> solo chiusura file
004700091215     c                   if        iinttla <> 'C'
004800091215     c                   exsr      elabora
004900011029     c                   endif
005000011011
005100091215     c                   if        iinttla <> *blanks
005200011031     c                   eval      *inlr = *on
005201091215     c
005202091215     C                   IF        Wtibs69r  <>  *BLANKS
005203091215     C                   eval      I69TLA  = 'C'
005204091215     C                   call      'TIBS69R'
005205091215     C                   parm                    TIBS69DS
005206091215     c                   ENDIF
005208090706      *
005300011029     c                   else
005400011031     c                   eval      *inrt = *on
005500011029     c                   endif
005600011011
005700060704      *----------------------------------------------------
005800060704      * IMPOSAZIONI INIZIALI
005900060704      *----------------------------------------------------
006000060704     C     IMPO          BEGSR
006100060704      * Controllo validità tipo lancio
006200091215     c                   if        iinttla <> 'C' and iinttla <> 'L' and
006300091215     c                             iinttla <> *blanks
006400091215     c                   clear                   iintTLA
006500060704     c                   endif
006600060704     C
006700091215     C                   IF        IintTLA<>'C'
007600091215     c* Se non ho ricevuto la ds1a la chaino col tipo incasso contrassegno
007601091215     c* altrimenti tengo buona la ds1a ricevuta
007602091215     c                   if        %parms<2
007700091215     C* chaino "1A" col tipo incasso ricevuto
007800091215     C                   MOVEL     '1A'          COD
007900091215     C                   MOVEL(P)  iinttic       KEY
008000060704     C                   EXSR      CTRTAB
008100091215     C  N32              MOVEL     TBLUNI        DS1A
008200091215     C   32              CLEAR                   DS1A
008201091216     c                   else
008202091216     c                   movel     ds1ap         ds1a
008203091215     c                   endif
008400060704     c
008500060704     c                   endif
008600060704     c
008700060704     c                   ENDSR
008800011011      *----------------------------------------------------
008900091215      * determina intestatrio assegno
009000011011      *----------------------------------------------------
009100091215     c     elabora       begsr
036800091215     c
036801091216    1c                   if        (§1afmb='M' and iinttip='I') or
036802091216     c                             iinttip='B'
036803091215     c* MITTENTE ORIGINALE
036804091215     c* tipo incasso che richiede mittente originale
036805091215    2c                   if        §1amro='S'
036806091216     c                   eval      ointrsci=iintrsco
036807091216     c                   if        iintrsco=*blanks
036808091216     c                   eval      ointerr='E'
036809091216     c                   eval      ointmsg='Manca mitt.originale'
036812091216     c                   endif
036813091216     c                   eval      ointtipi='O'
036814091215   x2c                   else
036815091215     c* Verifico se presente cod.int.assegno su fiar5 record "GEN"
036816091216     c                   clear                   w_cdi
036817091216    3c                   if        iintcdi=*blanks
036818091216   3ac                   if        iintaas>0
036819091215     c                   clear                   dar5gen
036820091215     c     kfiar5        chain     fiar501l
036821091216    4c                   if        %found(fiar501l)
036822091215     c                   movel     ar5uni        dar5gen
036823091216    4c                   endif
036824091216     c                   eval      w_cdi=§ar5cdi
036825091216   3ac                   endif
036827091216   x3c                   else
036828091216     c                   eval      w_cdi=iintcdi
036829091216    3c                   endif
036830091215     c* ORDINANTE
036831091216    3c                   if        w_cdi<>*blanks
036832091215     c* ..ragione sociale da cnaco
036833091215     C                   clear                   TIBS69DS
036834091216     C                   move      w_cdi         I69kac
036835091215     C                   call      'TIBS69R'
036836091215     C                   parm                    tibs69DS
036837091215     C                   parm                    ds_cnaco
036838091215     C                   parm                    ds_cnind
036839091215     C                   parm                    ds_cnclp
036840091215     C                   parm                    ds_fncls
036841091215     c                   eval      ointrsci=acorag
036842091216     c                   eval      ointtipi='R'
036843091216     c                   eval      ointcdi=w_cdi
036844091215     C                   eval      wtibs69r = 'S'
036845091215   x3c                   else
036846091215     c* intestatario=MITTENTE
036847091215     c                   eval      ointrsci=iintrscm
036848091216    4c                   if        iintrscm=*blanks
036849091216     c                   eval      ointerr='E'
036850091216     c                   eval      ointmsg='Manca mittente'
036851091216    4c                   endif
036852091216     c                   eval      ointtipi='M'
036853091216    3c                   endif
036854091215    2c                   endif
036855091216     c                   else
036856091216     c* se tipo incasso a corriere non passo nessun intestatario
036857091216     c                   eval      ointerr='C'
036858091215    1c                   endif
036900091215     c                   ENDSR
037000060704     c*
037100091215     C* CONTROLLO RECORD IN TABELLA ------------------------*
037200060704     C     CTRTAB        BEGSR
038600060704     c* controllo
038700060704     C     KTAB          CHAIN     TABEL                              32
038800060704     C  N32TBLFLG        IFNE      ' '
038900060704     C                   SETON                                        32
039000060704     C                   END
039300060704     C                   ENDSR
