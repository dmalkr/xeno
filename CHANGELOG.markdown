  1.0.0.0
	* Changed `process` function API to take a record of callbacks
	  (breaking change)
	* Generalized `validate` function type to take any `VectorizedString` instead
	  of just `ByteString`
	New features:
	* Added `VectorizedString` class, and using ByteStringNullTerminated
	  to make parsing 40-160% faster in common use case.
	* Added Xeno.DOM.Robust interface for parsing HTML, SGML documents
	  that may miss closing tags
	* Added Xeno.SAX.CPS interface for chaining event-based parsers

	0.3.5.2
	* Fix dependency lower bounds (GHC 8.0.1 is the earliest version currently supported)

	0.3.5
	* Improve error handling (#24 #26, mgajda)

	0.3.4
	* Fixed #14 and add test for #15
	* Fixed typos in the examples (unhammer)

	0.3.2
	Fixed DOM parsing from bystrings with non-zero offset (#11, qrilka)

	0.3
	Fixed name parsing (for attributes and tags) so it conforms with the XML spec (qrilka)
	Fixed parsing failure when root tag is preceded by white space (though without checking for white space characters specifically) (qrilka)
	Added contribution guidelines (ocramz)

	0.2
	Added CDATA support (Rembane)

	0.1
	First Hackage release
