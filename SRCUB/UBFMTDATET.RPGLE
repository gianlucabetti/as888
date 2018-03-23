000100111222     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('UBBNDDIR')
000200111222     fubfmtdatedcf   e             workstn
000300111227       // - Status ds
000400111227     d Status         sds
000500111227     d  SDSpgm           *proc
000600111227     d  SDSprm           *parms
000700111227     d  SDSusr               254    263
000800111227       // - InfDS
000900111227     d InfDspF         ds
001000111227     d  dsp_aid              369    369a
001100111227
001200111227       // - KPJBA
001300111227     d kpjba         e ds
001400120102     D*
001500120103     D* DS di ritorno per formato chiamata RE
001600120103     D pOutREDS        DS
001700120103     D  pOutREDateOut                64A
001800120103     D  pOutREError                   2A
001900120103     D  pOutREDescr                  34A
002000111227
002100111227       // - Parametri x Controllo profilo utenti
002200111227     d TIBS34ds      e ds
002300111227
002400111227     d §AzUte        e ds                  extname(AZUTE00F)
002500111227     d                                     dtaara
002600111227     d §DatiUte      e ds                  extname(dDatiUte)
002700111227     d                                     dtaara
002800111227       //--------------------------------------------------------------
002900111227       //?Definizione prototipi procedure usate.                       ?
003000111227       //--------------------------------------------------------------
003100111227       // - Reperimento dati utente
003200111227      /copy gaitrasrc/srcProtoPr,TIBS34R
003300111227     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003400111227     c     *Entry        plist
003500111227     c                   parm                    KPJBA
003600111227
003700111227
003800111227      /free
003900111227
004000111227         in(E) §AzUte;
004100111227         if NOT %error;
004200111227           in(E) §DatiUte;
004300111227         endif;
004400111227         if %error or RSut = *blanks;
004500111227           clear TIBS34ds;
004600111227           tibs34r(tibs34ds);
004700111227           in §AzUte;
004800111227           in §DatiUte;
004900111227         endif;
005000111227
005100111227      /end-free
005200111227
005300111227     C                   EVAL      V1Tpgm = SDSpgm
005400111227     C                   WRITE     T1
005500111227     C                   DO        *hival
005600111222     C                   EXFMT     D1
005700111222     C                   IF        *inkc = *on
005800111222     C                   SETON                                        LR
005900111222     C                   RETURN
006000111222     C                   ENDIF
006100111228     C                   EVAL      V1DMSG  = *blank
006200111228     C                   SETOFF                                       28
006300111228     C* controllo che le date di partenza siano in alternativa
006400111228     C                   IF        DtPart > 0 and DtPart_A <> *blank
006500111228     C                   EVAL      V1DMSG  =
006600111228     C                             'Valorizzare in alternativa le 2 date di par-
006700111228     C                             tenza'
006800111228     C                   SETON                                        28
006900111228     C                   ITER
007000111228     C                   ENDIF
007100111228     C                   IF        DtPart > 0
007200120103     C                   EVAL      pOutREDS =
007300111227     C                              UBFMTDATE_Convert
007400111227     C                               (%editc(DtPart:'X')
007500120103     C                                       : FmPart : FmArri : 'RE')
007600120103     C                   IF        pOutREError = *blank
007700120103     C                   EVAL      DtArri = pOutREDateOut
007800111228     C                   ELSE
007900120103     C                   EVAL      DtArri = pOutREDateOut
008000120103     C                   EVAL      V1DMSG  = pOutREDescr
008100120102     C                   SETON                                        28
008200120102     C                   ENDIF
008300120102     C*
008400120102     C                   ELSE
008500120103     C                   EVAL      pOutREDS =
008600111228     C                              UBFMTDATE_Convert
008700111228     C                               (DtPart_A
008800120103     C                                       : FmPart : FmArri : 'RE')
008900120103     C                   IF        pOutREError = *blank
009000120103     C                   EVAL      DtArri = pOutREDateOut
009100120102     C                   ELSE
009200120103     C                   EVAL      DtArri = pOutREDateOut
009300120103     C                   EVAL      V1DMSG  = pOutREDescr
009400120102     C                   SETON                                        28
009500120102     C                   ENDIF
009600120102     C*
009700111228     C                   ENDIF
009800111222     C                   ENDDO
009900111220     c                   seton                                        lr
