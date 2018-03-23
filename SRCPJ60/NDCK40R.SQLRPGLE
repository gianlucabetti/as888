000100140103     ***************************************************************************
000101140103     **
000102140103     ** Wrapper a PJCI1O3/NDCK40R.
000103140103     **
000104140103     ** Con l'attivazione di Piteco (novembre 2013) l'azione CK40 dettaglio da
000105140103     ** remote banking è usata solo sul rapporto bancario 0048 CCO su cui arrivano
000106140103     ** i bonifici che Piteco non riesce ad attribuire. E' importante registrare
000107140103     ** con data registrazione uguale a data operazione, in modo che sul conto
000108140103     ** 000204 00000048 ci sia lo stesso movimento in dare e avere nello stesso
000109140103     ** giorno.
000110140103     **
000111140103     ** Questo programma inserisce per l'utente che lo chiama una riga in ANMPE00F
000112140103     ** per forzare l'uso della data operazione come data registrazione.
000113140103     **
000114140103     ***************************************************************************
000115140103     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000116140103
000117140103     ***************************************************************************
000118140103     **
000119140103     ** Prototipi.
000120140103     **
000121140103     ***************************************************************************
000122140103      /COPY GAITRASRC/SRCPROTOPR,QUILNGTX
000123140103      /COPY GAITRASRC/SRCPROTOPR,XSOC
000124140103     D PJCI1O3_NDCK40R...
000125140103     D                 PR                  EXTPGM('PJCI1O3/NDCK40R')
000126140103     D  kpjba                              LIKEDS(kpjba)
000127140103
000128140103     ***************************************************************************
000129140103     **
000130140103     ** Costanti.
000131140103     **
000132140103     ***************************************************************************
000133140103     D SOCIETA_BRT     C                   '201'
000134140103     D MASTER          S             10A   INZ('EDPDC')
000135140103     D NDCK40R         S             10A   INZ('NDCK40R')
000136140103     D PV              S              2A   INZ('PV')
000137140103
000138140103     ***************************************************************************
000139140103     **
000140140103     ** Strutture dati.
000141140103     **
000142140103     ***************************************************************************
000143140103      /COPY QSYSINC/QRPGLESRC,QUSEC
000144140103     D kpjba         E DS                  QUALIFIED
000145140103     D xSoc001Ds     E DS                  QUALIFIED INZ
000147140103
000148140103     ***************************************************************************
000149140103     **
000150140103     ** Campi.
000151140103     **
000152140103     ***************************************************************************
000153140103     D count           S             20I 0
000154140103     D esitoProj       S              1A
000155140103     D textString      S            256A
000156140103
000157140103     ***************************************************************************
000158140103     **
000159140103     ** Parametri.
000160140103     **
000161140103     ***************************************************************************
000162140103     C     *ENTRY        PLIST
000163140103     C                   PARM                    kpjba
000164140103
000165140103     ***************************************************************************
000166140103     **
000167140103     ** Main.
000168140103     **
000169140103     ***************************************************************************
000170140103
000171140103      /FREE
000172140103
000173140103       *INLR = *ON;
000174140103
000175140103       Proj_Societa( 'SOC001'
000176140103                   : xSoc001Ds.xscSoc
000177140103                   : *ZERO
000178140103                   : *BLANK
000179140103                   : esitoProj
000180140103                   : xSoc001Ds
000181140103                   : kpjba
000182140103                   );
000183140103
000184140103       IF esitoProj = *ON OR xSoc001Ds.xscSoc <> SOCIETA_BRT;
000185140103         CLEAR qusec;
000186140103         qusbprv = %SIZE(qusec);
000187140103         textString = 'L''azione CK40 dettaglio da remote banking può essere +
000188140103                       eseguita solo nella società ' + SOCIETA_BRT + '.';
000189140103         DisplayLongText( textString
000190140103                        : %SIZE(textString)
000191140103                        : *BLANK
000192140103                        : *BLANK
000193140103                        : qusec
000194140103                        );
000195140103         RETURN;
000196140103       ENDIF;
000197140103
000198140103       IF kpjba.knmus <> MASTER;
000199140103
000200140103         EXEC SQL
000201140103           SELECT COUNT(*)
000202140103             INTO :count
000203140103             FROM anmpe00f
000204140103            WHERE mpeSoc = :xSoc001Ds.xscSoc
000205140103              AND mpePgm = :NDCK40R
000206140103              AND mpeTip = :PV
000207140103              AND mpePrf = :kpjba.knmus
000208140103              AND mpeCod = 0
000209140103         ;
000210140103
000211140103         IF count = *ZERO;
000212140103
000213140103           EXEC SQL
000214140103             INSERT INTO anmpe00f
000215140103               SELECT mpeSoc, mpeDes, mpePgm, :kpjba.knmus
000216140103                    , mpeCod, mpePar, mpeTip, mpeReg
000217140103                 FROM anmpe00f
000218140103                WHERE mpeSoc = :xSoc001Ds.xscSoc
000219140103                  AND mpePgm = :NDCK40R
000220140103                  AND mpeTip = :PV
000221140103                  AND mpePrf = :MASTER
000222140103                  AND mpeCod = 0
000223140103           ;
000224140103
000225140103         ELSE;
000226140103
000227140103           EXEC SQL
000228140103             UPDATE anmpe00f
000229140103                SET mpePar = ( SELECT mpePar
000230140103                                 FROM anmpe00f
000231140103                                WHERE mpeSoc = :xSoc001Ds.xscSoc
000232140103                                  AND mpePgm = :NDCK40R
000233140103                                  AND mpeTip = :PV
000234140103                                  AND mpePrf = :MASTER
000235140103                                  AND mpeCod = 0
000236140103                                FETCH FIRST 1 ROW ONLY
000237140103                             )
000238140103              WHERE mpeSoc = :xSoc001Ds.xscSoc
000239140103                AND mpePgm = :NDCK40R
000240140103                AND mpeTip = :PV
000241140103                AND mpePrf = :kpjba.knmus
000242140103                AND mpeCod = 0
000243140103           ;
000244140103
000245140103         ENDIF;
000246140103
000247140103       ENDIF;
000248140103
000249140103       PJCI1O3_NDCK40R( kpjba );
000250140103
000251140103      /END-FREE
