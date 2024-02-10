#' Generate URL for OPTED METEOR API
#'
#' This function generates a URL for accessing the OPTED METEOR API based on specified parameters.
#'
#' Currently, the package supports only API endpoints to retrieve data, i.e. not calls to edit entries.
#' Full documentation can be found here: \url{https://meteor.opted.eu/api/swagger}
#'
#' @param method The HTTP method to use. Default is "get".
#' @param ressource The resource to access.
#' @param option The option to specify within the resource.
#' @param n Number of results. Default is 50.
#' @param type Array of types.
#' @param type_connector Connector for type array.
#' @param audience_size Audience size.
#' @param audience_size_connector Connector for audience size.
#' @param audience_size_unit Unit for audience size.
#' @param audience_size_unit_connector Connector for audience size unit.
#' @param audience_size_recent Recent audience size.
#' @param audience_size_recent_connector Connector for recent audience size.
#' @param audience_size_recent_unit Unit for recent audience size.
#' @param audience_size_recent_unit_connector Connector for recent audience size unit.
#' @param channel Channel.
#' @param channel_connector Connector for channel.
#' @param contains_ads Indicator for ads.
#' @param country Country.
#' @param country_connector Connector for country.
#' @param countries Array of countries.
#' @param countries_connector Connector for countries.
#' @param date_founded Date founded.
#' @param date_founded_connector Connector for date founded.
#' @param geographic_scope Geographic scope.
#' @param geographic_scope_connector Connector for geographic scope.
#' @param languages Array of languages.
#' @param languages_connector Connector for languages.
#' @param name_abbrev Name abbreviation.
#' @param name_abbrev_connector Connector for name abbreviation.
#' @param ownership_kind Ownership kind.
#' @param payment_model Payment model.
#' @param party_affiliated Party affiliation.
#' @param publication_kind Publication kind.
#' @param publication_kind_connector Connector for publication kind.
#' @param publication_cycle Publication cycle.
#' @param sources_included Included sources.
#' @param sources_included_connector Connector for included sources.
#' @param subnational_scope Subnational scope.
#' @param subnational_scope_connector Connector for subnational scope.
#' @param transcript_kind Transcript kind.
#' @param topical_focus Topical focus.
#' @param website_allows_comments Indicator for allowing comments on website.
#' @param terms Search terms.
#' @param is_politician Indicator for politician.
#' @param verified_account Indicator for verified account.
#' @param special_interest Special interest.
#' @param defunct Indicator for defunct.
#' @param fulltext_available Indicator for fulltext availability.
#' @param graphical_user_interface Indicator for graphical user interface.
#' @param language_independent Indicator for language independence.
#' @param conditions_of_access Conditions of access.
#' @param text_types Array of text types.
#' @param text_types_connector Connector for text types.
#' @param text_units Array of text units.
#' @param text_units_connector Connector for text units.
#' @param modalities Array of modalities.
#' @param modalities_connector Connector for modalities.
#' @param meta_variables Array of meta variables.
#' @param meta_variables_connector Connector for meta variables.
#' @param concept_variables Array of concept variables.
#' @param concept_variables_connector Connector for concept variables.
#' @param date_published Date published.
#' @param date_published_connector Connector for date published.
#' @param platforms Array of platforms.
#' @param platforms_connector Connector for platforms.
#' @param programming_languages Array of programming languages.
#' @param programming_languages_connector Connector for programming languages.
#' @param open_source Indicator for open source.
#' @param used_for Used for.
#' @param used_for_connector Connector for used for.
#' @param designed_for Designed for.
#' @param designed_for_connector Connector for designed for.
#' @param input_file_format Input file format.
#' @param input_file_format_connector Connector for input file format.
#' @param output_file_format Output file format.
#' @param output_file_format_connector Connector for output file format.
#' @param author_validated Indicator for author validation.
#' @param validation_dataset Validation dataset.
#' @param validation_dataset_connector Connector for validation dataset.
#' @param methodologies Array of methodologies.
#' @param methodologies_connector Connector for methodologies.
#' @param datasets_used Array of datasets used.
#' @param datasets_used_connector Connector for datasets used.
#' @param channels Array of channels.
#' @param channels_connector Connector for channels.
#' @param entries_included Included entries.
#' @param entries_included_connector Connector for included entries.
#' @param authors Array of authors.
#' @param authors_connector Connector for authors.
#' @param add_params Additional parameters to append to the URL.
#' @param uid User ID.
#' @param page Page number.
#'
#' @return A character string representing the generated URL.
#'
#' @examples
#' generateURL(method = "get", ressource = "view", option = "random")
#'
#' @export
generateURL <- function(

  method = c(
    "get",
    "post"
  ),
  ressource = c(
    "external",
    "lookup",
    "query",
    "view"
  ),
  option = c(
    "count",
    "entry",
    "random",
    "recent",
    "ownership",
    "similar",
    "uid",
    "cran",
    "doi",
    "instagram",
    "telegram",
    "twitter",
    "vk",
    "website"
  ),
  n = 50,

  # Parameters that accept arrays as input:
  type = c(
    "Entry",
    "PoliticalParty",
    "Organization",
    "JournalisticBrand",
    "NewsSource",
    "Government",
    "Parliament",
    "Person",
    "Channel",
    "Country",
    "Multinational",
    "Subnational",
    "Archive",
    "Dataset",
    "Tool",
    "ScientificPublication",
    "Author",
    "Language",
    "ProgrammingLanguage",
    "Operation",
    "FileFormat",
    "MetaVariable",
    "ConceptVariable",
    "TextType",
    "UnitOfAnalysis",
    "Modality",
    "Collection",
    "LearningMaterial"
  ),
  type_connector = NULL,
  audience_size = NULL,
  audience_size_connector = NULL,
  audience_size_unit = NULL,
  audience_size_unit_connector = NULL,
  audience_size_recent = NULL,
  audience_size_recent_connector = NULL,
  audience_size_recent_unit = NULL,
  audience_size_recent_unit_connector = NULL,
  channel = NULL,
  channel_connector = NULL,
  contains_ads = c(
    "yes",
    "no",
    "non subscribers",
    "NA"
  ),
  country = NULL,
  country_connector = NULL,
  countries = NULL,
  countries_connector = NULL,
  date_founded = NULL,
  date_founded_connector = NULL,
  geographic_scope = c(
    "multinational",
    "national",
    "subnational"
  ),
  geographic_scope_connector = NULL,
  languages = NULL,
  languages_connector = NULL,
  name_abbrev = NULL,
  name_abbrev_connector = NULL,
  ownership_kind = c(
    "private ownership",
    "public ownership",
    "unkown",
    "NA"
  ),
  payment_model = c(
    "free",
    "partly free",
    "not free",
    "NA"
  ),
  party_affiliated = c(
    "yes",
    "no",
    "NA"
  ),
  publication_kind = c(
    "newspaper",
    "news site",
    "news agency",
    "tv show",
    "radio show",
    "podcast",
    "news blog",
    "alternative media",
    "organizational communication"
  ),
  publication_kind_connector = NULL,
  publication_cycle = c(
    "continuous",
    "daily",
    "multiple times per week",
    "weekly",
    "twice a month",
    "monthly",
    "less than monthly",
    "NA"
  ),
  sources_included = NULL,
  sources_included_connector = NULL,
  subnational_scope = NULL,
  subnational_scope_connector = NULL,
  transcript_kind = NULL, #
  topical_focus = c(
    "economy",
    "education",
    "environment",
    "health",
    "media",
    "politics",
    "religion",
    "society",
    "science",
    "youth"
  ),
  website_allows_comments = NULL,

  # Single input parameters
  terms = NULL,
  is_politician = NULL,
  verified_account = NULL,
  special_interest = NULL,

  # Tools
  defunct = NULL,
  fulltext_available = NULL,
  graphical_user_interface = NULL,
  language_independent = NULL,

  # Array
  conditions_of_access = c(
    "NA",
    "free",
    "registration",
    "request",
    "purchase"
  ),
  text_types = NULL,
  text_types_connector = NULL,
  text_units = NULL,
  text_units_connector = NULL,
  modalities = NULL,
  modalities_connector = NULL,
  meta_variables = NULL,
  meta_variables_connector = NULL,
  concept_variables = NULL,
  concept_variables_connector = NULL,
  date_published = NULL,
  date_published_connector = NULL,
  platforms = c(
    "windows",
    "linux",
    "macos"
  ),
  platforms_connector = NULL,
  programming_languages = NULL,
  programming_languages_connector = NULL,
  open_source = c(
    "NA",
    "yes",
    "no"
  ),
  used_for = NULL,
  used_for_connector = NULL,
  designed_for = NULL,
  designed_for_connector = NULL,
  input_file_format = NULL,
  input_file_format_connector = NULL,
  output_file_format = NULL,
  output_file_format_connector = NULL,
  author_validated = c(
    "NA",
    "yes",
    "no"
  ),
  validation_dataset = NULL,
  validation_dataset_connector = NULL,
  methodologies = NULL,
  methodologies_connector = NULL,
  datasets_used = NULL,
  datasets_used_connector = NULL,
  channels = NULL,
  channels_connector = NULL,
  entries_included = NULL,
  entries_included_connector = NULL,
  authors = NULL,
  authors_connector = NULL,

  # Pass other parameters:
  add_params = NULL,

  uid = NULL,

  page = 1
) {

  # Set base URL
  url <- "https://meteor.opted.eu/api/"

  # Add API endpoint ("ressource")
  url <- paste0(url, ressource)

  # Add API endpoint ("option")
  if(missing(option)) option = NULL
  if(!is.null(option)){
    url <- paste0(url, "/", option)
  }

  # GET requests

  if(method == "get"){


    # Params for ressource "query"

    if(ressource == "query"){


      # Add parameters
      url <- paste0(url, "?")


      # Single input parameters

      # Number of results
      if (!is.null(n)) {
        url <- paste0(url, "_max_results=", n)
      }

      # Page
      if (!is.null(page)) {
        url <- paste0(url, "&_page=", page)
      }

      # Terms
      if (!is.null(terms)) {
        url <- paste0(url, "&_terms=", terms)
      }

      # Is politician
      if (!is.null(is_politician)) {
        url <- paste0(url, "&is_politician=", is_politician)
      }

      # Is verified_account
      if (!is.null(verified_account )) {
        url <- paste0(url, "&verified_account =", verified_account )
      }

      # #Special interest
      if (!is.null(special_interest )) {
        url <- paste0(url, "&special_interest =", special_interest )
      }


      ### Tools: single input parameters

      # defunct
      if (!is.null(defunct)) {
        url <- paste0(url, "&defunct=", defunct)
      }

      # fulltext_available
      if (!is.null(fulltext_available)) {
        url <- paste0(url, "&fulltext_available=", fulltext_available)
      }

      # graphical_user_interface
      if (!is.null(graphical_user_interface)) {
        url <- paste0(url, "&graphical_user_interface=", graphical_user_interface)
      }

      # language_independent
      if (!is.null(language_independent)) {
        url <- paste0(url, "&language_independent=", language_independent)
      }

      # conditions_of_access
      if (missing(conditions_of_access)) conditions_of_access = NULL
      if (!is.null(conditions_of_access)) {
        url <- paste0(url, paste0("&conditions_of_access=", utils::URLencode(conditions_of_access), collapse = ""))
      }

      # open_source
      if (missing(open_source)) open_source = NULL
      if (!is.null(open_source)) {
        url <- paste0(url, paste0("&open_source=", utils::URLencode(open_source), collapse = ""))
      }

      # author_validated
      if (missing(author_validated)) author_validated = NULL
      if (!is.null(author_validated)) {
        url <- paste0(url, paste0("&author_validated=", utils::URLencode(author_validated), collapse = ""))
      }



      # Parameters allowing arrays

      # Dgraph Type
      if (!is.null(type)) {
        url <- paste0(url, paste0("&dgraph.type=", type, collapse = ""))
        if (!is.null(type_connector)) {
          url <- paste0(url, "&dgraph.type%2Aconnector=", type_connector)
        }else{
          url <- paste0(url, "&dgraph.type%2Aconnector=", "or") # set or as default
        }
      }

      # Audience size
      if (!is.null(audience_size_unit)) {
        url <- paste0(url, paste0("&audience_size%7Cunit=", utils::URLencode(audience_size_unit), collapse = ""))
        if (!is.null(audience_size_unit_connector)) {
          url <- paste0(url, "&audience_size%7Cunit%2Aconnector=", audience_size_unit_connector)
        }else{
          url <- paste0(url, "&audience_size%7Cunit%2Aconnector=", "OR") # set or as default
        }
        url <- paste0(url, paste0("&audience_size%7Ccount=", utils::URLencode(audience_size), collapse = ""))
        if (!is.null(audience_size_connector)) {
          url <- paste0(url, "&audience_size%7Ccount%2Aconnector=", audience_size_connector)
        }else{
          url <- paste0(url, "&audience_size%7Ccount%2Aconnector=", "gt") # set or as default
        }

      }

      # Audience size recent
      if (!is.null(audience_size_recent_unit)) {
        url <- paste0(url, paste0("&audience_size_recent%7Cunit=", utils::URLencode(audience_size_recent_unit), collapse = ""))
        if (!is.null(audience_size_recent_unit_connector)) {
          url <- paste0(url, "&audience_size_recent%7Cunit%2Aconnector=", audience_size_recent_unit_connector)
        }else{
          url <- paste0(url, "&audience_size_recent%7Cunit%2Aconnector=", "OR") # set or as default
        }
        url <- paste0(url, paste0("&audience_size_recent%7Ccount=", utils::URLencode(audience_size_recent), collapse = ""))
        if (!is.null(audience_size_recent_connector)) {
          url <- paste0(url, "&audience_size_recent%7Ccount%2Aconnector=", audience_size_recent_connector)
        }else{
          url <- paste0(url, "&audience_size_recent%7Ccount%2Aconnector=", "gt") # set or as default
        }

      }

      # Channel in hex
      if (!is.null(channel)) {
        url <- paste0(url, paste0("&channel=", channel, collapse = ""))
        if (!is.null(channel_connector)) {
          url <- paste0(url, "&channel%2Aconnector=", channel_connector)
        }else{
          url <- paste0(url, "&channel%2Aconnector=", "OR") # set or as default
        }
      }

      # Contains ads
      if (missing(contains_ads)) contains_ads = NULL
      if (!is.null(contains_ads)) {
        url <- paste0(url, paste0("&contains_ads=", utils::URLencode(contains_ads), collapse = ""))
      }


      # Country (in hex code)
      if (!is.null(country)) {
        url <- paste0(url, paste0("&country=", country, collapse = ""))
        if (!is.null(country_connector)) {
          url <- paste0(url, "&country%2Aconnector=", country_connector)
        }else{
          url <- paste0(url, "&country%2Aconnector=", "OR") # set or as default
        }
      }

      # Countries (in hex code)
      if (!is.null(countries)) {
        url <- paste0(url, paste0("&countries=", countries, collapse = ""))
        if (!is.null(countries_connector)) {
          url <- paste0(url, "&countries%2Aconnector=", countries_connector)
        }else{
          url <- paste0(url, "&countries%2Aconnector=", "OR") # set or as default
        }
      }

      # Date founded
      if (!is.null(date_founded)) {
        url <- paste0(url, paste0("&date_founded=", date_founded, collapse = ""))
        if (!is.null(date_founded_connector)) {
          url <- paste0(url, "&date_founded%2Aconnector=", date_founded_connector)
        }else{
          url <- paste0(url, "&date_founded%2Aconnector=", "OR") # set or as default
        }
      }


      # geographic scope
      if (missing(geographic_scope)) geographic_scope = NULL
      if (!is.null(geographic_scope))  {
        url <- paste0(url, paste0("&geographic_scope=", geographic_scope, collapse = ""))
        if (!is.null(geographic_scope_connector)) {
          url <- paste0(url, "&geographic_scope%2Aconnector=", geographic_scope_connector)
        }else{
          url <- paste0(url, "&geographic_scope%2Aconnector=", "OR") # set or as default
        }
      }


      # Languages
      if (!is.null(languages)) {
        url <- paste0(url, paste0("&languages=", languages, collapse = ""))
        if (!is.null(languages_connector)) {
          url <- paste0(url, "&languages%2Aconnector=", languages_connector)
        }else{
          url <- paste0(url, "&languages%2Aconnector=", "OR") # set or as default
        }
      }

      # Name abbrev
      if (!is.null(name_abbrev)) {
        url <- paste0(url, paste0("&name_abbrev=", name_abbrev, collapse = ""))
        if (!is.null(name_abbrev_connector)) {
          url <- paste0(url, "&name_abbrev%2Aconnector=", name_abbrev_connector)
        }else{
          url <- paste0(url, "&name_abbrev%2Aconnector=", "OR") # set or as default
        }
      }

      # Ownership kind
      if (missing(ownership_kind)) ownership_kind = NULL
      if (!is.null(ownership_kind)) {
        url <- paste0(url, paste0("&ownership_kind=", utils::URLencode(ownership_kind), collapse = ""))
      }

      # Payment model
      if (missing(payment_model)) payment_model = NULL
      if (!is.null(payment_model)){
        url <- paste0(url, paste0("&payment_model=", utils::URLencode(payment_model), collapse = ""))
      }


      # Party affiliated
      if (missing(party_affiliated)) party_affiliated = NULL
      if (!is.null(party_affiliated)) {
        url <- paste0(url, paste0("&party_affiliated=", utils::URLencode(party_affiliated), collapse = ""))
      }


      # publication_cycle
      if (missing(publication_cycle)) publication_cycle = NULL
      if (!is.null(publication_cycle))  {
        url <- paste0(url, paste0("&publication_cycle=", utils::URLencode(publication_cycle), collapse = ""))
      }


      # sources_included
      if (!is.null(sources_included)) {
        url <- paste0(url, paste0("&sources_included=", sources_included, collapse = ""))
        if (!is.null(sources_included_connector)) {
          url <- paste0(url, "&sources_included%2Aconnector=", sources_included)
        }else{
          url <- paste0(url, "&sources_included%2Aconnector=", "OR") # set or as default
        }
      }

      # subnational_scope
      if (!is.null(subnational_scope)) {
        url <- paste0(url, paste0("&subnational_scope=", subnational_scope, collapse = ""))
        if (!is.null(subnational_scope_connector)) {
          url <- paste0(url, "&subnational_scope%2Aconnector=", subnational_scope_connector)
        }else{
          url <- paste0(url, "&subnational_scope%2Aconnector=", "OR") # set or as default
        }
      }


      # Publication kind
      if (missing(publication_kind)) publication_kind = NULL
      if (!is.null(publication_kind))  {
        url <- paste0(url, paste0("&publication_kind=", utils::URLencode(publication_kind), collapse = ""))
        if (!is.null(publication_kind_connector)) {
          url <- paste0(url, "&publication_kind%2Aconnector=", publication_kind_connector)
        }else{
          url <- paste0(url, "&publication_kind%2Aconnector=", "OR") # set or as default
        }
      }

      # transcript_kind
      if (!is.null(transcript_kind)) {
        url <- paste0(url, paste0("&transcript_kind=", transcript_kind, collapse = ""))
      }


      # topical_focus
      if (missing(topical_focus)) topical_focus = NULL
      if (!is.null(topical_focus)) {
        url <- paste0(url, paste0("&topical_focus=", utils::URLencode(topical_focus), collapse = ""))
      }


      # website_allows_comments
      if (!is.null(website_allows_comments)) {
        url <- paste0(url, paste0("&website_allows_comments=", website_allows_comments, collapse = ""))
      }

      ### Tool parameters allowing arrays:

      # text_types
      if (!is.null(text_types)) {
        url <- paste0(url, paste0("&text_types=", utils::URLencode(text_types), collapse = ""))
        if (!is.null(text_types_connector)) {
          url <- paste0(url, "&text_types%2Aconnector=", text_types_connector)
        }else{
          url <- paste0(url, "&text_types%2Aconnector=", "OR") # set OR as default
        }
      }


      # text_units
      if (!is.null(text_units)) {
        url <- paste0(url, paste0("&text_units=", utils::URLencode(text_units), collapse = ""))
        if (!is.null(text_units_connector)) {
          url <- paste0(url, "&text_units%2Aconnector=", text_units_connector)
        }else{
          url <- paste0(url, "&text_units%2Aconnector=", "OR") # set OR as default
        }
      }


      # modalities
      if (!is.null(modalities)) {
        url <- paste0(url, paste0("&modalities=", utils::URLencode(modalities), collapse = ""))
        if (!is.null(modalities_connector)) {
          url <- paste0(url, "&modalities%2Aconnector=", modalities_connector)
        }else{
          url <- paste0(url, "&modalities%2Aconnector=", "OR") # set OR as default
        }
      }


      # meta_variables
      if (!is.null(meta_variables)) {
        url <- paste0(url, paste0("&meta_variables=", utils::URLencode(meta_variables), collapse = ""))
        if (!is.null(meta_variables_connector)) {
          url <- paste0(url, "&meta_variables%2Aconnector=", meta_variables_connector)
        }else{
          url <- paste0(url, "&meta_variables%2Aconnector=", "OR") # set OR as default
        }
      }


      # concept_variables
      if (!is.null(concept_variables)) {
        url <- paste0(url, paste0("&concept_variables=", utils::URLencode(concept_variables), collapse = ""))
        if (!is.null(concept_variables_connector)) {
          url <- paste0(url, "&concept_variables%2Aconnector=", concept_variables_connector)
        }else{
          url <- paste0(url, "&concept_variables%2Aconnector=", "OR") # set OR as default
        }
      }

      # date_published
      if (!is.null(date_published)) {
        url <- paste0(url, paste0("&date_published=", utils::URLencode(date_published), collapse = ""))
        if (!is.null(date_published_connector)) {
          url <- paste0(url, "&date_published%2Aconnector=", date_published_connector)
        }else{
          url <- paste0(url, "&date_published%2Aconnector=", "OR") # set OR as default
        }
      }


      # platforms
      if (missing(platforms)) platforms = NULL
      if (!is.null(platforms)) {
        url <- paste0(url, paste0("&platforms=", utils::URLencode(platforms), collapse = ""))
        if (!is.null(platforms_connector)) {
          url <- paste0(url, "&platforms%2Aconnector=", platforms_connector)
        }else{
          url <- paste0(url, "&platforms%2Aconnector=", "OR") # set OR as default
        }
      }


      # programming_languages
      if (!is.null(programming_languages)) {
        url <- paste0(url, paste0("&programming_languages=", utils::URLencode(programming_languages), collapse = ""))
        if (!is.null(programming_languages_connector)) {
          url <- paste0(url, "&programming_languages%2Aconnector=", programming_languages_connector)
        }else{
          url <- paste0(url, "&programming_languages%2Aconnector=", "OR") # set OR as default
        }
      }


      # used_for
      if (!is.null(used_for)) {
        url <- paste0(url, paste0("&used_for=", utils::URLencode(used_for), collapse = ""))
        if (!is.null(used_for_connector)) {
          url <- paste0(url, "&used_for%2Aconnector=", used_for_connector)
        }else{
          url <- paste0(url, "&used_for%2Aconnector=", "OR") # set OR as default
        }
      }


      # designed_for
      if (!is.null(designed_for)) {
        url <- paste0(url, paste0("&designed_for=", utils::URLencode(designed_for), collapse = ""))
        if (!is.null(designed_for_connector)) {
          url <- paste0(url, "&designed_for%2Aconnector=", designed_for_connector)
        }else{
          url <- paste0(url, "&designed_for%2Aconnector=", "OR") # set OR as default
        }
      }


      # input_file_format
      if (!is.null(input_file_format)) {
        url <- paste0(url, paste0("&input_file_format=", utils::URLencode(input_file_format), collapse = ""))
        if (!is.null(input_file_format_connector)) {
          url <- paste0(url, "&input_file_format%2Aconnector=", input_file_format_connector)
        }else{
          url <- paste0(url, "&input_file_format%2Aconnector=", "OR") # set OR as default
        }
      }


      # output_file_format
      if (!is.null(output_file_format)) {
        url <- paste0(url, paste0("&output_file_format=", utils::URLencode(output_file_format), collapse = ""))
        if (!is.null(output_file_format_connector)) {
          url <- paste0(url, "&output_file_format%2Aconnector=", output_file_format_connector)
        }else{
          url <- paste0(url, "&output_file_format%2Aconnector=", "OR") # set OR as default
        }
      }


      # validation_dataset
      if (!is.null(validation_dataset)) {
        url <- paste0(url, paste0("&validation_dataset=", utils::URLencode(validation_dataset), collapse = ""))
        if (!is.null(validation_dataset_connector)) {
          url <- paste0(url, "&validation_dataset%2Aconnector=", validation_dataset_connector)
        }else{
          url <- paste0(url, "&validation_dataset%2Aconnector=", "OR") # set OR as default
        }
      }

      # methodologies
      if (!is.null(methodologies)) {
        url <- paste0(url, paste0("&methodologies=", utils::URLencode(methodologies), collapse = ""))
        if (!is.null(methodologies_connector)) {
          url <- paste0(url, "&methodologies%2Aconnector=", methodologies_connector)
        }else{
          url <- paste0(url, "&methodologies%2Aconnector=", "OR") # set OR as default
        }
      }

      # datasets_used
      if (!is.null(datasets_used)) {
        url <- paste0(url, paste0("&datasets_used=", utils::URLencode(datasets_used), collapse = ""))
        if (!is.null(datasets_used_connector)) {
          url <- paste0(url, "&datasets_used%2Aconnector=", datasets_used_connector)
        }else{
          url <- paste0(url, "&datasets_used%2Aconnector=", "OR") # set OR as default
        }
      }

      # channels
      if (!is.null(channels)) {
        url <- paste0(url, paste0("&channels=", utils::URLencode(channels), collapse = ""))
        if (!is.null(channels_connector)) {
          url <- paste0(url, "&channels%2Aconnector=", channels_connector)
        }else{
          url <- paste0(url, "&channels%2Aconnector=", "OR") # set OR as default
        }
      }

      # entries_included
      if (!is.null(entries_included)) {
        url <- paste0(url, paste0("&entries_included=", utils::URLencode(entries_included), collapse = ""))
        if (!is.null(entries_included_connector)) {
          url <- paste0(url, "&entries_included%2Aconnector=", entries_included_connector)
        }else{
          url <- paste0(url, "&entries_included%2Aconnector=", "OR") # set OR as default
        }
      }

      # authors
      if (!is.null(authors)) {
        url <- paste0(url, paste0("&authors=", utils::URLencode(authors), collapse = ""))
        if (!is.null(authors_connector)) {
          url <- paste0(url, "&authors%2Aconnector=", authors_connector)
        }else{
          url <- paste0(url, "&authors%2Aconnector=", "OR") # set OR as default
        }
      }


    }


    if (!is.null(add_params)) {
      url <- paste0(url, "&", add_params)
    }



  }

  # Add uid
  if(!is.null(uid)){
    url <- paste0(url, "/", uid)
  }


  return(url)

}




#' Generate Body for API Call
#'
#' This function generates the body for a POST method API call based on the specified parameters.
#'
#' @param package A character string specifying the package name.
#' @param doi A character string specifying the DOI identifier.
#' @param fresh A logical value indicating whether the data should be fetched fresh.
#' @param handle A character string specifying the social media handle.
#' @param body_url A character string specifying the URL.
#'
#' @return A list representing the generated body for the API call.
#'
#' @examples
#' body <- generate_body(package = "dplyr")
#'
#' @export
generate_body <- function(
    package = NULL,
    doi = NULL,
    fresh = TRUE,
    handle = NULL,
    body_url = NULL
){

  # null default
  body = NULL

  # cran
  if(!is.null(package)){
    body = list(package = package)
  }

  # doi
  if(!is.null(doi)){
    body = list(fresh = fresh,
                identifier = doi)
  }

  # social media
  if(!is.null(handle)){
    body = list(handle = handle)
  }

  # website
  if(!is.null(body_url)){
    body = list(url = body_url)
  }

  return(body)

}




#' Call API Function
#'
#' This function makes a call to the specified API URL using the provided HTTP method and returns the response.
#'
#' @param url A character string representing the URL of the API call.
#' @param method A character string indicating the HTTP method to be used ("get" or "post").
#' @param body A list representing the body of the request, required for POST requests.
#'
#' @return The response from the API call.
#'
#' @examples
#' res <- callAPI(method = "get",
#'               url = generateURL(method = "get", ressource = "view", option = "random"))
#'
#' @export
callAPI <- function(url,
                    method = c("get", "post"),
                    body = NULL
                    ){

  # Stop if no url
  if (is.null(url)){
    stop("Error: No URL specified.")
  }

  if(method == "get"){

    # GET call API
    tryCatch(
      {
        raw <- httr::GET(url)
        content <- httr::content(raw)
      },
      error = function(e) {
        message(paste0("An error occurred while calling API: ", e))
        NA
      }
    )

  }

  if(method == "post"){

    body = body

    # Stop if no body
    if (is.null(body)){
      stop("Error: No body specified.")
    }

    # POST call API
    tryCatch(
      {

        raw <- httr::POST(url = url, body = body, encode = "json")
        content <- httr::content(raw)

      },
      error = function(e) {
        message(paste0("An error occurred while calling API: ", e))
        NA
      }
    )

  }

  return(content)

}




#' Paginated API Call
#'
#' This function paginates through the results of the provided API URL based on the specified HTTP method and returns the aggregated response.
#'
#' @param url A character string representing the URL of the API call.
#' @param method A character string indicating the HTTP method to be used ("get" or "post").
#' @param verbose Logical indicating whether to display progress messages.
#' @param n An integer specifying the number of results per page.
#' @param n_max An integer specifying the maximum number of results to fetch.
#'
#' @return The aggregated response from paginating through the API call.
#'
#' @examples
#' pag <- paginateAPI(url= generateURL(method = "get", ressource = "view", option = "random"),
#'                     method = "get")
#'
#' @export
paginateAPI <- function(url,
                        method = method,
                        verbose = TRUE,
                        n = 50,
                        n_max = Inf){

  # First API call
  content <- callAPI(url = url, method = method)

  # Check amount of data
  len <- length(content); if (verbose) cat(len, "obs. ")

  total <- length(content)

  # Check if there's more
  while (len==n && total < n_max){

    # waiting before making the next API call...
    Sys.sleep(0.1)

    # Add + 1 to the current page number
    new_page <- as.character(as.numeric(regmatches(url, gregexpr("(?<=&_page=)([0-9]+)", url, perl = TRUE))) + 1 )
    # Replace in url
    url <- gsub("(?<=&_page=)([0-9]+)", new_page, url, perl = TRUE)

    # Fetch content
    nxt_content <- callAPI(url = url, method = method)

    # Calculate content lengths
    total <- total + length(nxt_content)
    len <- length(nxt_content); if (verbose) cat("+", len, "obs. ")

    # Append content lists
    content <- base::append(content, nxt_content)

  }

  return(content)

}




#' Generic GET Function
#'
#' This function performs a GET request to retrieve data from the specified API resource using pagination.
#'
#' @param method A character string specifying the HTTP method to be used ("get" by default).
#' @param ressource A character string specifying the type of resource to retrieve data from ("external", "lookup", "query", "view").
#' @param verbose Logical indicating whether to display progress messages (default is TRUE).
#' @param n_max An integer specifying the maximum number of results to fetch (default is Inf).
#' @param n An integer specifying the number of results per page (default is 50).
#' @param format A character string indicating the format of the output ("dataframe" or "raw").
#' @param ... Additional arguments passed to the function for generating the URL.
#'
#' @return The retrieved data in the specified format.
#'
#' @examples
#' news_sources <- get_generic(method = "get",
#'                            ressource = "query",
#'                            type = "NewsSource",
#'                            countries = c("0x1b"),
#'                            geographic_scope = "national",
#'                            n_max = 10,
#'                            n = 10,
#'                            format = "dataframe"
#'                            )
#'
#' @export
get_generic <- function(method = "get",
                        ressource = c("external",
                                      "lookup",
                                      "query",
                                      "view"),
                        verbose = TRUE,
                        n_max = Inf,
                        n = 50,
                        format = c("dataframe", "raw"),
                        ...){

  # Generate url, pass additional arguments
  url <- generateURL(method = method,
                     ressource = ressource,
                     n = n,
                     ...)

  # Make paginated API call, optionally using a second API key
  con <- paginateAPI(url = url,
                     method = method,
                     verbose = verbose,
                     n = n,
                     n_max= n_max)

  # Check output format
  format <- match.arg(format, c("dataframe", "raw"))

  if(format == "dataframe"){
    con <- fleece::rectangularize(con)
    return(con)
  } else {
    return(con)
  }

}



#' Generic POST Function
#'
#' This function performs a POST request to submit data to the specified API resource.
#'
#' @param method A character string specifying the HTTP method to be used ("post" by default).
#' @param ressource A character string specifying the type of resource to submit data to ("external" by default).
#' @param option A character string specifying the specific option for the resource ("cran", "doi", "instagram", "telegram", "twitter", "vk", "website").
#' @param package A character string specifying the package name (required for "cran" option).
#' @param doi A character string specifying the DOI (required for "doi" option).
#' @param fresh Logical indicating freshness (TRUE by default).
#' @param handle A character string specifying the handle (required for "instagram", "telegram", "twitter", "vk" options).
#' @param website A character string specifying the website URL (required for "website" option).
#' @param format A character string indicating the format of the output ("dataframe" or "raw").
#'
#' @return The response from the API call in the specified format.
#'
#' @examples
#' dplyr <- post_generic(option = "cran", package = "dplyr", format = "dataframe")
#'
#' @export
post_generic <- function(method = "post",
                        ressource = "external",
                        option = c("cran",
                                   "doi",
                                   "instagram",
                                   "telegram",
                                   "twitter",
                                   "vk",
                                   "website"),
                        package = NULL,
                        doi = NULL,
                        fresh = TRUE,
                        handle = NULL,
                        website = NULL,
                        format = c("dataframe", "raw")
                        ){

  # Generate URL, pass additional arguments
  url <- generateURL(method = method,
                     ressource = ressource,
                     option = option)

  # Generate body for the request
  body <- generate_body(package = package,
                        doi = doi,
                        fresh = fresh,
                        body_url = website,
                        handle = handle)

  # Make POST API call
  con <- callAPI(url = url,
                 method = method,
                 body = body
                )

  # Check output format
  format <- match.arg(format, c("dataframe", "raw"))

  if(format == "dataframe"){
    con <- fleece::rectangularize(con)
    return(con)
  } else if(format == "raw") {
    return(con)
  }

}






#' Call Meteor API Function
#'
#' This function performs API calls to the Meteor API with customizable parameters for both GET and POST requests.
#'
#' Currently, the package supports only API endpoints to retrieve data, i.e. not calls to edit entries.
#' Full documentation can be found here: \url{https://meteor.opted.eu/api/swagger}
#'
#' @param method A character string specifying the HTTP method to be used ("get" by default).
#' @param ressource A character string specifying the type of resource to interact with ("external", "lookup", "query", "view").
#' @param option A character string specifying additional options for the resource.
#' @param type A character string specifying the type of resource.
#' @param type_connector A character string specifying the connector for type.
#' @param audience_size An integer specifying the audience size.
#' @param audience_size_connector A character string specifying the connector for audience size.
#' @param audience_size_unit A character string specifying the unit for audience size.
#' @param audience_size_unit_connector A character string specifying the connector for audience size unit.
#' @param audience_size_recent An integer specifying the recent audience size.
#' @param audience_size_recent_connector A character string specifying the connector for recent audience size.
#' @param audience_size_recent_unit A character string specifying the unit for recent audience size.
#' @param audience_size_recent_unit_connector A character string specifying the connector for recent audience size unit.
#' @param channel A character string specifying the channel.
#' @param channel_connector A character string specifying the connector for channel.
#' @param contains_ads A character string specifying if the resource contains ads.
#' @param country A character string specifying the country.
#' @param country_connector A character string specifying the connector for country.
#' @param countries A character string specifying the countries.
#' @param countries_connector A character string specifying the connector for countries.
#' @param date_founded A character string specifying the date founded.
#' @param date_founded_connector A character string specifying the connector for date founded.
#' @param geographic_scope A character string specifying the geographic scope.
#' @param geographic_scope_connector A character string specifying the connector for geographic scope.
#' @param languages A character string specifying the languages.
#' @param languages_connector A character string specifying the connector for languages.
#' @param name_abbrev A character string specifying the name abbreviation.
#' @param name_abbrev_connector A character string specifying the connector for name abbreviation.
#' @param ownership_kind A character string specifying the ownership kind.
#' @param payment_model A character string specifying the payment model.
#' @param party_affiliated A character string specifying if party affiliated.
#' @param publication_kind A character string specifying the publication kind.
#' @param publication_kind_connector A character string specifying the connector for publication kind.
#' @param publication_cycle A character string specifying the publication cycle.
#' @param sources_included A character string specifying the sources included.
#' @param sources_included_connector A character string specifying the connector for sources included.
#' @param subnational_scope A character string specifying the subnational scope.
#' @param subnational_scope_connector A character string specifying the connector for subnational scope.
#' @param transcript_kind A character string specifying the transcript kind.
#' @param topical_focus A character string specifying the topical focus.
#' @param website_allows_comments A character string specifying if the website allows comments.
#' @param terms A character string specifying the terms.
#' @param is_politician A character string specifying if the entity is a politician.
#' @param verified_account A character string specifying if the account is verified.
#' @param special_interest A character string specifying special interests.
#' @param defunct A character string specifying if the resource is defunct.
#' @param fulltext_available A character string specifying if full text is available.
#' @param graphical_user_interface A character string specifying if a graphical user interface is available.
#' @param language_independent A character string specifying if the resource is language independent.
#' @param conditions_of_access A character string specifying the conditions of access.
#' @param text_types A character string specifying the text types.
#' @param text_types_connector A character string specifying the connector for text types.
#' @param text_units A character string specifying the text units.
#' @param text_units_connector A character string specifying the connector for text units.
#' @param modalities A character string specifying the modalities.
#' @param modalities_connector A character string specifying the connector for modalities.
#' @param meta_variables A character string specifying the meta variables.
#' @param meta_variables_connector A character string specifying the connector for meta variables.
#' @param concept_variables A character string specifying the concept variables.
#' @param concept_variables_connector A character string specifying the connector for concept variables.
#' @param date_published A character string specifying the date published.
#' @param date_published_connector A character string specifying the connector for date published.
#' @param platforms A character string specifying the platforms.
#' @param platforms_connector A character string specifying the connector for platforms.
#' @param programming_languages A character string specifying the programming languages.
#' @param programming_languages_connector A character string specifying the connector for programming languages.
#' @param open_source A character string specifying if the resource is open source.
#' @param used_for A character string specifying what the resource is used for.
#' @param used_for_connector A character string specifying the connector for what the resource is used for.
#' @param designed_for A character string specifying what the resource is designed for.
#' @param designed_for_connector A character string specifying the connector for what the resource is designed for.
#' @param input_file_format A character string specifying the input file format.
#' @param input_file_format_connector A character string specifying the connector for input file format.
#' @param output_file_format A character string specifying the output file format.
#' @param output_file_format_connector A character string specifying the connector for output file format.
#' @param author_validated A character string specifying if the author is validated.
#' @param validation_dataset A character string specifying the validation dataset.
#' @param validation_dataset_connector A character string specifying the connector for validation dataset.
#' @param methodologies A character string specifying the methodologies.
#' @param methodologies_connector A character string specifying the connector for methodologies.
#' @param datasets_used A character string specifying the datasets used.
#' @param datasets_used_connector A character string specifying the connector for datasets used.
#' @param channels A character string specifying the channels.
#' @param channels_connector A character string specifying the connector for channels.
#' @param entries_included A character string specifying the entries included.
#' @param entries_included_connector A character string specifying the connector for entries included.
#' @param authors A character string specifying the authors.
#' @param authors_connector A character string specifying the connector for authors.
#' @param add_params Additional parameters to pass.
#' @param uid A character string specifying the user ID.
#' @param page An integer specifying the page number (default is 1).
#' @param package A character string specifying the package name.
#' @param doi A character string specifying the DOI.
#' @param fresh A logical value indicating freshness (default is TRUE).
#' @param handle A character string specifying the handle.
#' @param website A character string specifying the website URL.
#' @param verbose A logical value indicating whether to display progress messages (default is TRUE).
#' @param n_max An integer specifying the maximum number of results to fetch (default is Inf).
#' @param n An integer specifying the number of results per page (default is 50).
#' @param format A character string indicating the format of the output ("dataframe" or "raw").
#' @param unnest_cutoff An integer specifying the cutoff point for unnesting the results.
#'
#' @return The retrieved data in the specified format.
#'
#' @examples
#' np <- call_meteor(method = "get",
#'                   ressource = "query",
#'                   type = "NewsSource",
#'                   countries = c("0x1b", "0x2f"),
#'                   format = "dataframe",
#'                   publication_kind = "newspaper",
#'                   n_max = 10,
#'                   n = 10)
#'
#' @export
call_meteor <- function(

  # URL PARAMETERS
  method = c(
    "get",
    "post"
  ),
  ressource = c(
    "external",
    "lookup",
    "query",
    "view"
  ),
  option = c(
    "count",
    "entry",
    "random",
    "recent",
    "ownership",
    "similar",
    "uid",
    "cran",
    "doi",
    "instagram",
    "telegram",
    "twitter",
    "vk",
    "website"
  ),

  # Parameters that accept arrays as input:
  type = c(
    "Entry",
    "PoliticalParty",
    "Organization",
    "JournalisticBrand",
    "NewsSource",
    "Government",
    "Parliament",
    "Person",
    "Channel",
    "Country",
    "Multinational",
    "Subnational",
    "Archive",
    "Dataset",
    "Tool",
    "ScientificPublication",
    "Author",
    "Language",
    "ProgrammingLanguage",
    "Operation",
    "FileFormat",
    "MetaVariable",
    "ConceptVariable",
    "TextType",
    "UnitOfAnalysis",
    "Modality",
    "Collection",
    "LearningMaterial"
  ),
  type_connector = NULL,
  audience_size = NULL,
  audience_size_connector = NULL,
  audience_size_unit = NULL,
  audience_size_unit_connector = NULL,
  audience_size_recent = NULL,
  audience_size_recent_connector = NULL,
  audience_size_recent_unit = NULL,
  audience_size_recent_unit_connector = NULL,
  channel = NULL,
  channel_connector = NULL,
  contains_ads = c(
    "yes",
    "no",
    "non subscribers",
    "NA"
  ),
  country = NULL,
  country_connector = NULL,
  countries = NULL,
  countries_connector = NULL,
  date_founded = NULL,
  date_founded_connector = NULL,
  geographic_scope = c(
    "multinational",
    "national",
    "subnational"
  ),
  geographic_scope_connector = NULL,
  languages = NULL,
  languages_connector = NULL,
  name_abbrev = NULL,
  name_abbrev_connector = NULL,
  ownership_kind = c(
    "private ownership",
    "public ownership",
    "unkown",
    "NA"
  ),
  payment_model = c(
    "free",
    "partly free",
    "not free",
    "NA"
  ),
  party_affiliated = c(
    "yes",
    "no",
    "NA"
  ),
  publication_kind = c(
    "newspaper",
    "news site",
    "news agency",
    "tv show",
    "radio show",
    "podcast",
    "news blog",
    "alternative media",
    "organizational communication"
  ),
  publication_kind_connector = NULL,
  publication_cycle = c(
    "continuous",
    "daily",
    "multiple times per week",
    "weekly",
    "twice a month",
    "monthly",
    "less than monthly",
    "NA"
  ),
  sources_included = NULL,
  sources_included_connector = NULL,
  subnational_scope = NULL,
  subnational_scope_connector = NULL,
  transcript_kind = NULL, #
  topical_focus = c(
    "economy",
    "education",
    "environment",
    "health",
    "media",
    "politics",
    "religion",
    "society",
    "science",
    "youth"
  ),
  website_allows_comments = NULL,

  # Single input parameters
  terms = NULL,
  is_politician = NULL,
  verified_account = NULL,
  special_interest = NULL,

  # Tools
  defunct = NULL,
  fulltext_available = NULL,
  graphical_user_interface = NULL,
  language_independent = NULL,

  # Array
  conditions_of_access = c(
    "NA",
    "free",
    "registration",
    "request",
    "purchase"
  ),
  text_types = NULL,
  text_types_connector = NULL,
  text_units = NULL,
  text_units_connector = NULL,
  modalities = NULL,
  modalities_connector = NULL,
  meta_variables = NULL,
  meta_variables_connector = NULL,
  concept_variables = NULL,
  concept_variables_connector = NULL,
  date_published = NULL,
  date_published_connector = NULL,
  platforms = c(
    "windows",
    "linux",
    "macos"
  ),
  platforms_connector = NULL,
  programming_languages = NULL,
  programming_languages_connector = NULL,
  open_source = c(
    "NA",
    "yes",
    "no"
  ),
  used_for = NULL,
  used_for_connector = NULL,
  designed_for = NULL,
  designed_for_connector = NULL,
  input_file_format = NULL,
  input_file_format_connector = NULL,
  output_file_format = NULL,
  output_file_format_connector = NULL,
  author_validated = c(
    "NA",
    "yes",
    "no"
  ),
  validation_dataset = NULL,
  validation_dataset_connector = NULL,
  methodologies = NULL,
  methodologies_connector = NULL,
  datasets_used = NULL,
  datasets_used_connector = NULL,
  channels = NULL,
  channels_connector = NULL,
  entries_included = NULL,
  entries_included_connector = NULL,
  authors = NULL,
  authors_connector = NULL,

  # Pass other parameters:
  add_params = NULL,
  uid = NULL,
  page = 1,

  ### GENERATE BODY PARAMS
  package = NULL,
  doi = NULL,
  fresh = TRUE,
  handle = NULL,
  website = NULL,


  ### PAGINATION PARAMS
  verbose = TRUE,
  n_max = Inf,
  n = 50,

  # Parsing
  format = c("dataframe",
             "raw"),
  unnest_cutoff = NULL

  ){

    # Stop if no method
    if (missing(method)){
      stop("Error: No method specified.")
    }
    # Stop if no ressource
    if (missing(ressource)){
      stop("Error: No ressource specified.")
    }


  # Replace 'drop down' arguments if missing with NULL
  if(missing(option)) option = NULL
  if(missing(type)) type = NULL
  if(missing(contains_ads)) contains_ads = NULL
  if(missing(geographic_scope)) geographic_scope = NULL
  if(missing(ownership_kind)) ownership_kind = NULL
  if(missing(payment_model)) payment_model = NULL
  if(missing(party_affiliated)) party_affiliated = NULL
  if(missing(publication_kind)) publication_kind = NULL
  if(missing(publication_cycle)) publication_cycle = NULL
  if(missing(topical_focus)) topical_focus = NULL
  if(missing(conditions_of_access)) conditions_of_access = NULL
  if(missing(platforms)) platforms = NULL
  if(missing(open_source)) open_source = NULL
  if(missing(author_validated)) author_validated = NULL



  # Generate url
  url <- generateURL(
    method = method,
    ressource = ressource,
    option = option,
    n = n,
    type = type,
    type_connector = type_connector,
    audience_size = audience_size,
    audience_size_connector = audience_size_connector,
    audience_size_unit = audience_size_unit,
    audience_size_unit_connector = audience_size_unit_connector,
    audience_size_recent = audience_size_recent,
    audience_size_recent_connector = audience_size_recent_connector,
    audience_size_recent_unit = audience_size_recent_unit,
    audience_size_recent_unit_connector = audience_size_recent_unit_connector,
    channel = channel,
    channel_connector = channel_connector,
    contains_ads = contains_ads,
    country = country,
    country_connector = country_connector,
    countries = countries,
    countries_connector = countries_connector,
    date_founded = date_founded,
    date_founded_connector = date_founded_connector,
    geographic_scope = geographic_scope,
    geographic_scope_connector = geographic_scope_connector,
    languages = languages,
    languages_connector = languages_connector,
    name_abbrev = name_abbrev,
    name_abbrev_connector = name_abbrev_connector,
    ownership_kind = ownership_kind,
    payment_model = payment_model,
    party_affiliated = party_affiliated,
    publication_kind = publication_kind,
    publication_kind_connector = publication_kind_connector,
    publication_cycle = publication_cycle,
    sources_included = sources_included,
    sources_included_connector = sources_included_connector,
    subnational_scope = subnational_scope,
    subnational_scope_connector = subnational_scope_connector,
    transcript_kind = transcript_kind, #
    topical_focus = topical_focus,
    website_allows_comments = website_allows_comments,
    terms = terms,
    is_politician = is_politician,
    verified_account = verified_account,
    special_interest = special_interest,
    defunct = defunct,
    fulltext_available = fulltext_available,
    graphical_user_interface = graphical_user_interface,
    language_independent = language_independent,
    conditions_of_access = conditions_of_access,
    text_types = text_types,
    text_types_connector = text_types_connector,
    text_units = text_units,
    text_units_connector = text_units_connector,
    modalities = modalities,
    modalities_connector = modalities_connector,
    meta_variables = meta_variables,
    meta_variables_connector = meta_variables_connector,
    concept_variables = concept_variables,
    concept_variables_connector = concept_variables_connector,
    date_published = date_published,
    date_published_connector = date_published_connector,
    platforms = platforms,
    platforms_connector = platforms_connector,
    programming_languages = programming_languages,
    programming_languages_connector = programming_languages_connector,
    open_source = open_source,
    used_for = used_for,
    used_for_connector = used_for_connector,
    designed_for = designed_for,
    designed_for_connector = designed_for_connector,
    input_file_format = input_file_format,
    input_file_format_connector = input_file_format_connector,
    output_file_format = output_file_format,
    output_file_format_connector = output_file_format_connector,
    author_validated = author_validated,
    validation_dataset = validation_dataset,
    validation_dataset_connector = validation_dataset_connector,
    methodologies = methodologies,
    methodologies_connector = methodologies_connector,
    datasets_used = datasets_used,
    datasets_used_connector = datasets_used_connector,
    channels = channels,
    channels_connector = channels_connector,
    entries_included = entries_included,
    entries_included_connector = entries_included_connector,
    authors = authors,
    authors_connector = authors_connector,
    add_params = add_params,
    uid = uid,
    page = page

  )


  if(method == "get"){

    # Make paginated API call
    con <- paginateAPI(
      url = url,
      method = method,
      verbose = verbose,
      n = n,
      n_max= n_max
      )

  }else{

    # For Method POST

    body <- generate_body(
      package = package,
      doi = doi,
      fresh = fresh,
      body_url = website,
      handle = handle
      )

    # Make paginated API call
    con <- callAPI(
      url = url,
      method = method,
      body = body
    )
  }



  # Check which output format
  format <- match.arg(format, c("dataframe", "raw"))

  if(format == "dataframe"){

    if(!is.null(unnest_cutoff)){
      con <- purrr::keep(con, ~ length(.x)<=unnest_cutoff)
    }

    con <- fleece::rectangularize(con)

    return(con)

    }else{

    return(con)

    }

}


