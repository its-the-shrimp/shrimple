## Testing

### Executing
To run all test samples & compare them to expected outputs, run `cargo test`.
The test suite compares `dist` in every sample directory to `expected-dist`, both directories.
If `expected-dist` is missing, it's copied from `dist`. If a `BLESS` environment variable
is provided to `cargo test`, its contents are replaced with the contents of `dist`
if they don't match.

### Creating
To create a test, add a directory to `tests/samples` with the name of the test spelled
only using alphanumeric characters and underscores.
In the directory, build out a website sample you want to test. Ensure its root is `index.html`.
Do not create `expected-dist` as it will be created automatically if it's missing.
