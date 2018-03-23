000001140828     D*
000002140828     D*--------------------------------------------------
000003140828     D* Procedure name: PropostaBlocco_bls_
000004140828     D* Purpose:        .
000005140828     D* Returns:        Esito.
000006140828     D* Parameter:      piKpjba => KPJBA
000007140828     D*--------------------------------------------------
000008140828     D PropostaBlocco_BLS_INIT...
000009140828     D                 PR            10I 0
000010140828     D  piKpjba                            LIKEDS(kpjba)
000011140828     D                                     OPTIONS(*NOPASS)
000012140828     D*--------------------------------------------------
000102140827     D* Procedure name: PropostaBlocco_bls_
000103140827     D* Purpose:        .
000104140827     D* Returns:        Esito.
000105140827     D* Parameter:      piKpjba => KPJBA
000106140827     D*--------------------------------------------------
000107140827     D PropostaBlocco_BLS_crea_proposta...
000108140827     D                 PR            10I 0
000109150120     D pisocieta                      3    value
000110150120     D piunita                        3    value
000111150120     D pipartitaiva                  20    value
000112150120     D piidcliente                    8    value
000113150120     D picodfiscale                  20    value
000114150120     D piscaduto                     11s 2 value
000115150120     D piesposizione                 11s 2 value
000116141110     D pitipo                         1    const
000117141110     D*--------------------------------------------------
000123141110     D* Procedure name: PropostaBlocco_bls_
000124141110     D* Purpose:        .
000125141110     D* Returns:        Esito.
000126141110     D* Parameter:      piKpjba => KPJBA
000127141110     D*--------------------------------------------------
000128141110     D PropostaBlocco_BLS_aut_crea_proposta...
000129141110     D                 PR            10I 0
000130150120     D pisocieta                      3    value
000132150120     D piunita                        3    value
000133150120     D pipartitaiva                  20    value
000134150120     D piidcliente                    8    value
000135150120     D picodfiscale                  20    value
000136150120     D piscaduto                     11s 2 value
000137141110     D pitipo                         1    const
000138141110     D*--------------------------------------------------
000139141002     D* Procedure name: PropostaBlocco_bls_
000300140827     D* Purpose:        .
000400080218     D* Returns:        Esito.
000500080609     D* Parameter:      piKpjba => KPJBA
000600080218     D*--------------------------------------------------
000601140828     D*
000700140827     D PropostaBlocco_BLS_richiesta_deroga_mora...
000701140827     D                 PR            10I 0
000900140828     D  piidproposta                 10I 0
000901141006     D  picausale                     2    OPTIONS(*NOPASS) const
000902141003     D  pinota                      257    OPTIONS(*NOPASS) const
000903141110     D*--------------------------------------------------
000928141110     D* Procedure name: PropostaBlocco_bls_
000929141110     D* Purpose:        .
000930141110     D* Returns:        Esito.
000931141110     D* Parameter:      piKpjba => KPJBA
000932141110     D*--------------------------------------------------
000933141110     D*
000934141110     D PropostaBlocco_BLS_Aut_richiesta_deroga_mora...
000935141110     D                 PR            10I 0
000936141110     D  piidproposta                 10I 0
000937141110     D  picausale                     2    OPTIONS(*NOPASS) const
000938141110     D  pinota                      257    OPTIONS(*NOPASS) const
000939141110     D*--------------------------------------------------
001002140827     D* Procedure name: PropostaBlocco_bls_
001003140827     D* Purpose:        .
001004140827     D* Returns:        Esito.
001005140827     D* Parameter:      piKpjba => KPJBA
001006140827     D*--------------------------------------------------
001007140827     D PropostaBlocco_BLS_accetta_deroga_mora...
001008140827     D                 PR            10I 0
001009140828     D  piidproposta                 10I 0
001010141006     D  picausale                     2    OPTIONS(*NOPASS) const
001011141003     D  pinota                      257    OPTIONS(*NOPASS) const
001014150915     D*--------------------------------------------------
001015150915     D* Procedure name: PropostaBlocco_bls_
001016150915     D* Purpose:        .
001017150915     D* Returns:        Esito.
001018150915     D* Parameter:      piKpjba => KPJBA
001019150915     D*--------------------------------------------------
001020150915     D PropostaBlocco_BLS_Check_Numero_deroghe...
001021150915     D                 PR            10I 0
001022150915     D  piidproposta                 10I 0
001023150915     D*--------------------------------------------------
001024141110     D* Procedure name: PropostaBlocco_bls_
001025141110     D* Purpose:        .
001026141110     D* Returns:        Esito.
001027141110     D* Parameter:      piKpjba => KPJBA
001028141110     D*--------------------------------------------------
001029141110     D PropostaBlocco_BLS_aut_accetta_deroga_mora...
001030141110     D                 PR            10I 0
001031141110     D  piidproposta                 10I 0
001032141110     D  picausale                     2    OPTIONS(*NOPASS) const
001033141110     D  pinota                      257    OPTIONS(*NOPASS) const
001034141110     D*--------------------------------------------------
001035140827     D* Procedure name: PropostaBlocco_bls_
001036140827     D* Purpose:        .
001037140827     D* Returns:        Esito.
001038140827     D* Parameter:      piKpjba => KPJBA
001039140827     D*--------------------------------------------------
001040140827     D PropostaBlocco_BLS_accetta_deroga_DG...
001041140827     D                 PR            10I 0
001042140828     D  piidproposta                 10I 0
001043141006     D  picausale                     2    OPTIONS(*NOPASS) const
001044141003     D  pinota                      257    OPTIONS(*NOPASS) const
001045141110     D*--------------------------------------------------
001046141110     D* Procedure name: PropostaBlocco_bls_
001047141110     D* Purpose:        .
001048141110     D* Returns:        Esito.
001049141110     D* Parameter:      piKpjba => KPJBA
001050141110     D*--------------------------------------------------
001051141110     D PropostaBlocco_BLS_aut_accetta_deroga_DG...
001052141110     D                 PR            10I 0
001053141110     D  piidproposta                 10I 0
001054141110     D  picausale                     2    OPTIONS(*NOPASS) const
001055141110     D  pinota                      257    OPTIONS(*NOPASS) const
001056141110     D*--------------------------------------------------
001057140827     D* Procedure name: PropostaBlocco_bls_
001058140827     D* Purpose:        .
001059140827     D* Returns:        Esito.
001060140827     D* Parameter:      piKpjba => KPJBA
001061140827     D*--------------------------------------------------
001062140827     D PropostaBlocco_BLS_rifiuta_deroga_mora...
001063140827     D                 PR            10I 0
001064140828     D  piidproposta                 10I 0
001065141006     D  picausale                     2    OPTIONS(*NOPASS) const
001066141003     D  pinota                      257    OPTIONS(*NOPASS) const
001067141110     D*--------------------------------------------------
001068141110     D* Procedure name: PropostaBlocco_bls_
001069141110     D* Purpose:        .
001070141110     D* Returns:        Esito.
001071141110     D* Parameter:      piKpjba => KPJBA
001072141110     D*--------------------------------------------------
001073141110     D PropostaBlocco_BLS_aut_rifiuta_deroga_mora...
001074141110     D                 PR            10I 0
001075141110     D  piidproposta                 10I 0
001076141110     D  picausale                     2    OPTIONS(*NOPASS) const
001077141110     D  pinota                      257    OPTIONS(*NOPASS) const
001078141110     D*--------------------------------------------------
001079140827     D* Procedure name: PropostaBlocco_bls_
001080140827     D* Purpose:        .
001081140827     D* Returns:        Esito.
001082140827     D* Parameter:      piKpjba => KPJBA
001083140827     D*--------------------------------------------------
001084140904     D PropostaBlocco_BLS_invia_mora...
001085140827     D                 PR            10I 0
001086140828     D  piidproposta                 10I 0
001087150407     D*--------------------------------------------------
001088141202     D* Procedure name: PropostaBlocco_bls_
001089141202     D* Purpose:        .
001090141202     D* Returns:        Esito.
001091141202     D* Parameter:      piKpjba => KPJBA
001092141202     D*--------------------------------------------------
001093141202     D PropostaBlocco_BLS_Aut_invia_mora...
001094141202     D                 PR            10I 0
001095141202     D  piidproposta                 10I 0
001096141202     D*--------------------------------------------------
001097140827     D* Procedure name: PropostaBlocco_bls_
001098140827     D* Purpose:        .
001099140827     D* Returns:        Esito.
001100140827     D* Parameter:      piKpjba => KPJBA
001101140827     D*--------------------------------------------------
001102140827     D PropostaBlocco_BLS_richiesta_deroga_blocco...
001103140827     D                 PR            10I 0
001104140828     D  piidproposta                 10I 0
001105141006     D  picausale                     2    OPTIONS(*NOPASS) const
001106141003     D  pinota                      257    OPTIONS(*NOPASS) const
001107141110     D*--------------------------------------------------
001108141110     D* Procedure name: PropostaBlocco_bls_
001109141110     D* Purpose:        .
001110141110     D* Returns:        Esito.
001111141110     D* Parameter:      piKpjba => KPJBA
001112141110     D*--------------------------------------------------
001113141110     D PropostaBlocco_BLS_aut_richiesta_deroga_blocco...
001114141110     D                 PR            10I 0
001115141110     D  piidproposta                 10I 0
001116141110     D  picausale                     2    OPTIONS(*NOPASS) const
001117141110     D  pinota                      257    OPTIONS(*NOPASS) const
001118141110     D*--------------------------------------------------
001119140827     D* Procedure name: PropostaBlocco_bls_
001120140827     D* Purpose:        .
001121140827     D* Returns:        Esito.
001122140827     D* Parameter:      piKpjba => KPJBA
001123140827     D*--------------------------------------------------
001124140827     D PropostaBlocco_BLS_rifiuta_deroga_blocco...
001125140827     D                 PR            10I 0
001126140828     D  piidproposta                 10I 0
001127141006     D  picausale                     2    OPTIONS(*NOPASS) const
001128141003     D  pinota                      257    OPTIONS(*NOPASS) const
001129141110     D*--------------------------------------------------
001130141110     D* Procedure name: PropostaBlocco_bls_
001131141110     D* Purpose:        .
001132141110     D* Returns:        Esito.
001133141110     D* Parameter:      piKpjba => KPJBA
001134141110     D*--------------------------------------------------
001135141110     D PropostaBlocco_BLS_aut_rifiuta_deroga_blocco...
001136141110     D                 PR            10I 0
001137141110     D  piidproposta                 10I 0
001138141110     D  picausale                     2    OPTIONS(*NOPASS) const
001139141110     D  pinota                      257    OPTIONS(*NOPASS) const
001140141110     D*--------------------------------------------------
001141140827     D* Procedure name: PropostaBlocco_bls_
001142140827     D* Purpose:        .
001143140827     D* Returns:        Esito.
001144140827     D* Parameter:      piKpjba => KPJBA
001145140827     D*--------------------------------------------------
001146140827     D PropostaBlocco_BLS_blocca_servizio...
001147140827     D                 PR            10I 0
001148141006     D  piidproposta                 10I 0
001149141110     D*--------------------------------------------------
001150141110     D* Procedure name: PropostaBlocco_bls_
001151141110     D* Purpose:        .
001152141110     D* Returns:        Esito.
001153141110     D* Parameter:      piKpjba => KPJBA
001154141110     D*--------------------------------------------------
001155141110     D PropostaBlocco_BLS_Aut_blocca_servizio...
001156141110     D                 PR            10I 0
001157141110     D  piidproposta                 10I 0
001158141222     D*--------------------------------------------------
001159141222     D* Procedure name: PropostaBlocco_bls_
001160141222     D* Purpose:        .
001161141222     D* Returns:        Esito.
001162141222     D* Parameter:      piKpjba => KPJBA
001163141222     D*--------------------------------------------------
001164141222     D PropostaBlocco_BLS_revoca...
001165141222     D                 PR            10I 0
001166141222     D  piidproposta                 10I 0
001167141222     D*--------------------------------------------------
001168141222     D* Procedure name: PropostaBlocco_bls_
001169141222     D* Purpose:        .
001170141222     D* Returns:        Esito.
001171141222     D* Parameter:      piKpjba => KPJBA
001172141222     D*--------------------------------------------------
001173141222     D PropostaBlocco_BLS_Aut_revoc...
001174141222     D                 PR            10I 0
001175141222     D  piidproposta                 10I 0
001176141222     D*--------------------------------------------------
001177140827     D* Procedure name: PropostaBlocco_bls_
001178140827     D* Purpose:        .
001179140827     D* Returns:        Esito.
001180140827     D* Parameter:      piKpjba => KPJBA
001181140827     D*--------------------------------------------------
001182140827     D PropostaBlocco_BLS_annulla_proposta...
001183140827     D                 PR            10I 0
001184140828     D  piidproposta                 10I 0
001185141006     D  picausale                     2    OPTIONS(*NOPASS) const
001186141006     D  pinota                      257    OPTIONS(*NOPASS) const
001187141029     D*--------------------------------------------------
001188141029     D* Procedure name: PropostaBlocco_bls_
001189141029     D* Purpose:        .
001190141029     D* Returns:        Esito.
001191141029     D* Parameter:      piKpjba => KPJBA
001192141029     D*--------------------------------------------------
001193141029     D*
001194141029     D PropostaBlocco_BLS_UNDO...
001195141029     D                 PR            10I 0
001196141029     D  piidproposta                 10I 0
001197141106     D*--------------------------------------------------
001198141106     D* Procedure name: PropostaBlocco_bls_
001199141106     D* Purpose:        .
001200141106     D* Returns:        Esito.
001201141106     D* Parameter:      piKpjba => KPJBA
001202141106     D*--------------------------------------------------
001203141106     D PropostaBlocco_BLS_UpdateSaldo...
001204141106     D                 PR            10I 0
001205141106     D  piidproposta                 10I 0
001206141126     d  piscaduto                          like(yblspr00f.impscaatt)
001207141126     d  piesposizione                      like(yblspr00f.impespatt)
001208141202     D*--------------------------------------------------
001209141202     D* Procedure name: PropostaBlocco_bls_
001210141202     D* Purpose:
001211141202     D* Returns:        Esito.
001212141202     D* Parameter:      piKpjba => KPJBA
001213141202     D*--------------------------------------------------
001214141202     D PropostaBlocco_BLS_chkSemaforo...
001215141202     D                 PR            10I 0
001216150407     D*--------------------------------------------------
001217150407     D* Procedure name: PropostaBlocco_bls_
001218150407     D* Purpose:        .
001219150407     D* Returns:        Esito.
001220150407     D* Parameter:      piKpjba => KPJBA
001221150407     D*--------------------------------------------------
001222150407     D PropostaBlocco_BLS_set_tipo_manuale...
001223150407     D                 PR            10I 0
001224150407     D  piidproposta                 10I 0
001225150408     D  picommit                      1    const
001226150407     D*--------------------------------------------------
001227150407     D* Procedure name: PropostaBlocco_bls_
001228150407     D* Purpose:        .
001229150407     D* Returns:        Esito.
001230150407     D* Parameter:      piKpjba => KPJBA
001231150407     D*--------------------------------------------------
001232150407     D PropostaBlocco_BLS_Aut_set_tipo_manuale...
001233150407     D                 PR            10I 0
001234150407     D  piidproposta                 10I 0
001235150407
