module.exports = {
  displayName: 'e2e:fast',
  moduleFileExtensions: ['js'],
  testMatch: ['<rootDir>/test-e2e/**/*.test.js'],
  testEnvironment: 'node',
  globalSetup: '<rootDir>/test-e2e/test/jestGlobalSetup.js',
  globalTeardown: '<rootDir>/test-e2e/test/jestGlobalTeardown.js',
  moduleFileExtensions: ['js', 'json'],
  modulePathIgnorePatterns: [
    '<rootDir>/esy-install/',
    '<rootDir>/node_modules/',
    '<rootDir>/test-e2e/build/fixtures/',
  ],
};
